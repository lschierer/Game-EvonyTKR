import { NestedStack, Stack } from 'aws-cdk-lib';
import * as ec2 from 'aws-cdk-lib/aws-ec2';
//import * as iam from 'aws-cdk-lib/aws-iam';
import * as cloudwatch from 'aws-cdk-lib/aws-cloudwatch';
import * as cloudwatch_actions from 'aws-cdk-lib/aws-cloudwatch-actions';
import * as sns from 'aws-cdk-lib/aws-sns';
import * as route53 from 'aws-cdk-lib/aws-route53';
import * as cdk from 'aws-cdk-lib';

import { CustomUbuntuUserData } from './userdata';

import { type MojoliciousStackProps } from './main-stack';

export interface UbuntuInstanceProps extends MojoliciousStackProps {
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

    const custominit = new CustomUbuntuUserData(this, props);

    custominit.prefix_etc_asset.grantRead(instanceRole);
    custominit.prefix_bin_asset.grantRead(instanceRole);

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
      userDataCausesReplacement: true,
      init: custominit.init,
      initOptions: custominit.initOptions,
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
