import { NestedStack, Stack } from 'aws-cdk-lib';
import * as ec2 from 'aws-cdk-lib/aws-ec2';
//import * as iam from 'aws-cdk-lib/aws-iam';
import * as s3Assets from 'aws-cdk-lib/aws-s3-assets';
import * as cloudwatch from 'aws-cdk-lib/aws-cloudwatch';
import * as cloudwatch_actions from 'aws-cdk-lib/aws-cloudwatch-actions';
import * as sns from 'aws-cdk-lib/aws-sns';
import * as route53 from 'aws-cdk-lib/aws-route53';
import * as fs from 'fs';
import * as cdk from 'aws-cdk-lib';
import * as yaml from 'js-yaml';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

import { type MojoliciousStackProps } from './main-stack';

interface UbuntuInstanceProps extends MojoliciousStackProps {
  vpc: ec2.IVpc | ec2.Vpc;
}

export class UbuntuInstance extends NestedStack {
  // return the instance to the parent so that I can get its IP address
  readonly instance: ec2.Instance;

  // return the hostname to the parent so that I can create DNS records
  // these will be required for certbot to run.
  readonly hostname: string;
  constructor(
    scope: NestedStack | Stack,
    id: string,
    props: UbuntuInstanceProps,
  ) {
    super(scope, id);

    const instanceRole = new cdk.aws_iam.Role(this, 'InstanceRole', {
      assumedBy: new cdk.aws_iam.ServicePrincipal('ec2.amazonaws.com'),
    });

    // slightly randomize the hostname so that if I need to iterate
    // on the way the ec2 instance is built, letsencrypt sees different account names
    this.hostname =
      props.environment === 'prod'
        ? `www${this.getRandomInteger(10, 99)}`
        : `${props.appSubdomain}${this.getRandomInteger(10, 99)}`;
    let userDataFile = fs.readFileSync(
      path.join(__dirname, '../etc/user-data.yaml'),
      'utf8',
    );
    userDataFile = userDataFile.replaceAll('REPLACEHOSTNAME', this.hostname);
    userDataFile = userDataFile.replaceAll(
      'REPLACE2.',
      props.environment === 'prod' ? '' : `${props.environment}2.`,
    );
    userDataFile = userDataFile.replaceAll(
      'REPLACEMODE',
      props.environment === 'prod' ? 'production' : 'staging',
    );

    const cloud_user_data: Record<string, unknown> = yaml.load(
      userDataFile,
    ) as Record<string, unknown>;
    cloud_user_data.runcmd =
      cloud_user_data['runcmd' as keyof typeof cloud_user_data] || [];

    const prefixBinAsset = new s3Assets.Asset(this, 'prefixBinAsset', {
      path: path.join(__dirname, '../prefixBin'),
    });
    prefixBinAsset.grantRead(instanceRole);

    const prefix_etc_asset = new s3Assets.Asset(this, 'prefix_etc_asset', {
      path: path.join(__dirname, '../etc'),
    });

    const shellCommands = ec2.UserData.forLinux();
    shellCommands.addCommands(
      'curl "https://awscli.amazonaws.com/awscli-exe-linux-aarch64.zip" -o "awscliv2.zip"',
      'unzip awscliv2.zip',
      'sudo ./aws/install',
    );

    const local_bin_path = shellCommands.addS3DownloadCommand({
      bucket: prefixBinAsset.bucket,
      bucketKey: prefixBinAsset.s3ObjectKey,
    });

    const local_etc_path = shellCommands.addS3DownloadCommand({
      bucket: prefix_etc_asset.bucket,
      bucketKey: prefix_etc_asset.s3ObjectKey,
    });

    const hostprefix =
      props.environment === 'prod' ? 'production' : props.environment;
    shellCommands.addCommands(
      'mkdir -p /opt/prefix/bin',
      'cd /opt/prefix/bin',
      `unzip ${local_bin_path}`,
      'chown -R appuser:www-data /opt/prefix',
      'chmod +x /opt/prefix/bin/*.sh',
      'mv .bash* /opt/prefix/',
      'cp /opt/prefix/bin/deploy-prefix.sh /usr/local/bin',
      'chmod 0755 /opt/prefix/bin/deploy-prefix.sh',
      `sed -i -E 's/replace/${hostprefix}/' /opt/prefix/bin/setup-cert.sh`,
      'cp /opt/prefix/bin/setup-cert.sh /usr/local/bin',
      'chmod 0755 /usr/local/bin/setup-cert.sh',
      'mkdir -p /tmp/prefix_etc',
      'cd /tmp/prefix_etc',
      `unzip ${local_etc_path}`,
      'sudo cp /tmp/prefix_etc/setup-cert.service /etc/systemd/system/',
      'sudo cp /tmp/prefix_etc/evonytkr.service /etc/systemd/system/',
      'sudo cp /tmp/prefix_etc/sysctl.conf /etc/sysctl.d/evonytkr.conf',
      'mkdir -p /opt/prefix/.local/share/mise/',
      'sudo cp /tmp/prefix_etc/trusted.toml /opt/prefix/.local/share/mise/trusted.toml',
      'sudo chown -R appuser:appuser /opt/prefix/.local',
      'sudo service procps force-reload',
    );

    // Generate mode-specific config BEFORE bootstrap (git clone happens in bootstrap)
    // Created in /opt/prefix/etc, then bootstrap.sh copies it to /opt/prefix/app after clone
    const appMode = props.environment === 'prod' ? 'production' : 'staging';
    const configFileName = `game-evony_t_k_r.${appMode}.yml`;
    console.log(`config file is ${configFileName}`);
    shellCommands.addCommands(
      'mkdir -p /opt/prefix/etc',
      'chown appuser:www-data /opt/prefix/etc',
      'cd /opt/prefix/etc',
    );

    // Run bootstrap and signal success/failure to CloudFormation
    // bootstrap.sh will: 1) git clone, 2) copy config from /opt/prefix/etc to /opt/prefix/app
    shellCommands.addCommands(
      'set +e', // Don't exit on error so we can signal failure
      'sudo -u appuser -s /bin/bash -l -c /opt/prefix/bin/bootstrap.sh',
      'BOOTSTRAP_EXIT_CODE=$?',
      'set -e',
    );

    shellCommands.addCommands(
      'sudo systemctl enable setup-cert.service',
      'systemctl start setup-cert.service',
      'sudo /opt/prefix/bin/setup-nginx.sh',
      'sudo systemctl enable evonytkr.service',
      'systemctl start evonytkr',
      'systemctl reload nginx',
    );

    (cloud_user_data.runcmd as Array<string>).push(shellCommands.render());

    // Combine them with MultiPart
    const userData = ec2.UserData.custom(
      `#cloud-config\n${yaml.dump(cloud_user_data)}`,
    );

    const instanceSize = !props.environment.localeCompare('dev')
      ? ec2.InstanceSize.LARGE
      : ec2.InstanceSize.LARGE;

    const ec2SecGroup = new ec2.SecurityGroup(
      this,
      `Mojo-${props.environment}-SecurityGroup`,
      {
        vpc: props.vpc,
      },
    );

    this.instance = new ec2.Instance(this, 'Instance', {
      role: instanceRole,
      userData,
      userDataCausesReplacement: true,
      vpc: props.vpc,
      instanceType: ec2.InstanceType.of(ec2.InstanceClass.T4G, instanceSize),
      securityGroup: ec2SecGroup,
      machineImage: this.genericLinuxImage(),
      vpcSubnets: { subnetType: ec2.SubnetType.PUBLIC },
      blockDevices: [
        {
          deviceName: '/dev/sda1',
          volume: ec2.BlockDeviceVolume.ebs(30, {
            volumeType: ec2.EbsDeviceVolumeType.GP3,
            deleteOnTermination: true,
          }),
        },
      ],
      // Remove resourceSignalTimeout - let instance succeed when it comes up
      // Bootstrap continues in background via systemd service
    });

    //(cloud_user_data.runcmd as Array<string>).push(shellCommands.render());

    ec2SecGroup.addIngressRule(
      ec2.Peer.anyIpv4(),
      ec2.Port.tcp(80),
      'httpIpv4',
    );
    ec2SecGroup.addIngressRule(
      ec2.Peer.anyIpv6(),
      ec2.Port.tcp(80),
      'httpIpv6',
    );
    ec2SecGroup.addIngressRule(
      ec2.Peer.anyIpv4(),
      ec2.Port.tcp(443),
      'httpsIpv4',
    );
    ec2SecGroup.addIngressRule(
      ec2.Peer.anyIpv6(),
      ec2.Port.tcp(443),
      'httpsIpv6',
    );
    ec2SecGroup.addIngressRule(ec2.Peer.anyIpv4(), ec2.Port.tcp(22), 'ssh');
    ec2SecGroup.addIngressRule(ec2.Peer.anyIpv6(), ec2.Port.tcp(22), 'ssh');

    // Production health monitoring
    if (props.environment === 'prod') {
      const topic = new sns.Topic(this, 'HealthAlertTopic', {
        displayName: 'Production Instance Health Alerts',
      });

      // EC2 status check alarm
      const statusAlarm = new cloudwatch.Alarm(this, 'InstanceHealthAlarm', {
        metric: new cloudwatch.Metric({
          namespace: 'AWS/EC2',
          metricName: 'StatusCheckFailed',
          dimensionsMap: {
            InstanceId: this.instance.instanceId,
          },
          statistic: 'Maximum',
          period: cdk.Duration.minutes(1),
        }),
        threshold: 1,
        evaluationPeriods: 2,
        datapointsToAlarm: 2,
        treatMissingData: cloudwatch.TreatMissingData.BREACHING,
      });
      statusAlarm.addAlarmAction(new cloudwatch_actions.SnsAction(topic));

      // Application health check
      const healthCheck = new route53.CfnHealthCheck(this, 'AppHealthCheck', {
        healthCheckConfig: {
          type: 'HTTPS',
          resourcePath: '/health',
          fullyQualifiedDomainName: `${props.appSubdomain}.${props.domainName}`,
          port: 443,
          requestInterval: 30,
          failureThreshold: 3,
        },
      });

      const healthAlarm = new cloudwatch.Alarm(this, 'AppHealthAlarm', {
        metric: new cloudwatch.Metric({
          namespace: 'AWS/Route53',
          metricName: 'HealthCheckStatus',
          dimensionsMap: {
            HealthCheckId: healthCheck.attrHealthCheckId,
          },
          statistic: 'Minimum',
          period: cdk.Duration.minutes(1),
        }),
        threshold: 1,
        evaluationPeriods: 2,
        comparisonOperator: cloudwatch.ComparisonOperator.LESS_THAN_THRESHOLD,
        treatMissingData: cloudwatch.TreatMissingData.BREACHING,
      });
      healthAlarm.addAlarmAction(new cloudwatch_actions.SnsAction(topic));
    }
  }

  genericLinuxImage() {
    const ubuntuCompanyOwnerId = '099720109477';
    // NOTE only pick LTS versions for your sanity!
    const ubuntuName = 'noble';

    const machineImage = ec2.MachineImage.genericLinux({
      [this.region]: new ec2.LookupMachineImage({
        // `YEAR-ARCH` are the first two stars
        name: `ubuntu/images/hvm-ssd-gp3/ubuntu-${ubuntuName}-*-*-server-*`,
        owners: [ubuntuCompanyOwnerId],
        filters: {
          architecture: [ec2.InstanceArchitecture.ARM_64],
          'image-type': ['machine'],
          state: ['available'],
          'root-device-type': ['ebs'],
          'virtualization-type': ['hvm'],
        },
      }).getImage(this).imageId,
    });

    return machineImage;
  }

  getRandomInteger(min: number, max: number): number {
    min = Math.ceil(min);
    max = Math.floor(max);
    return Math.floor(Math.random() * (max - min + 1)) + min;
  }
}
