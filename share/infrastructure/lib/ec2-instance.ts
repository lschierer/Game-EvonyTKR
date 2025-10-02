import { NestedStack, Stack } from 'aws-cdk-lib';
import * as ec2 from 'aws-cdk-lib/aws-ec2';
import * as s3Assets from 'aws-cdk-lib/aws-s3-assets';
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

    const cloud_user_data: Record<string, unknown> = yaml.load(
      userDataFile,
    ) as Record<string, unknown>;
    cloud_user_data.runcmd =
      cloud_user_data['runcmd' as keyof typeof cloud_user_data] || [];
    const mojoBinAsset = new s3Assets.Asset(this, 'MyScriptAsset', {
      path: path.join(__dirname, '../mojobin'),
    });
    mojoBinAsset.grantRead(instanceRole);
    const shellCommands = ec2.UserData.forLinux();
    shellCommands.addCommands(
      'curl "https://awscli.amazonaws.com/awscli-exe-linux-aarch64.zip" -o "awscliv2.zip"',
      'unzip awscliv2.zip',
      'sudo ./aws/install',
    );
    const localPath = shellCommands.addS3DownloadCommand({
      bucket: mojoBinAsset.bucket,
      bucketKey: mojoBinAsset.s3ObjectKey,
    });
    shellCommands.addCommands(
      'mkdir -p /opt/mojo/bin',
      'cd /opt/mojo/bin',
      `unzip ${localPath}`,
      'chown -R mojo:mojo /opt/mojo',
      'chmod +x /opt/mojo/bin/*.sh',
      'mv .bash* /opt/mojo/',
      'cp /opt/mojo/bin/deploy-mojo.sh /usr/local/bin',
      'chmod 0755 /opt/mojo/bin/deploy-mojo.sh',
      'sudo -u mojo -s /bin/bash -l -c /opt/mojo/bin/bootstrap.sh',
    );
    shellCommands.addCommands(
      'systemctl enable mojolicious',
      'systemctl enable mojolicious-worker',
      'systemctl start mojolicious-worker',
      'systemctl start mojolicious',
      'systemctl reload nginx',
    );
    (cloud_user_data.runcmd as Array<string>).push(shellCommands.render());

    // Combine them with MultiPart
    const userData = ec2.UserData.custom(
      `#cloud-config\n${yaml.dump(cloud_user_data)}`,
    );

    const instanceSize = !props.environment.localeCompare('dev')
      ? ec2.InstanceSize.MEDIUM
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
      instanceType: ec2.InstanceType.of(
        ec2.InstanceClass.BURSTABLE4_GRAVITON,
        instanceSize,
      ),
      securityGroup: ec2SecGroup,
      machineImage: this.genericLinuxImage(),
      vpcSubnets: { subnetType: ec2.SubnetType.PUBLIC },
    });

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
