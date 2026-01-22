import { NestedStack, Stack } from 'aws-cdk-lib';
import * as ec2 from 'aws-cdk-lib/aws-ec2';
//import * as iam from 'aws-cdk-lib/aws-iam';
import * as cloudwatch from 'aws-cdk-lib/aws-cloudwatch';
import * as cloudwatch_actions from 'aws-cdk-lib/aws-cloudwatch-actions';
import * as sns from 'aws-cdk-lib/aws-sns';
import * as route53 from 'aws-cdk-lib/aws-route53';
import * as cdk from 'aws-cdk-lib';

import { CustomUbuntuUserData } from './userdata';

import { type ApplicationStackProps } from './main-stack';

export interface UbuntuInstanceProps extends ApplicationStackProps {
  vpc: ec2.IVpc | ec2.Vpc;
}

const genericLinuxImage = (scope: Stack | NestedStack) => {
  const ubuntuCompanyOwnerId = '099720109477';
  // NOTE only pick LTS versions for your sanity!
  const ubuntuName = 'noble';

  const machineImage = ec2.MachineImage.genericLinux({
    [scope.region]: new ec2.LookupMachineImage({
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
    }).getImage(scope).imageId,
  });

  return machineImage;
};

const getRandomInteger = (min: number, max: number): number => {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
};

export class UbuntuInstance extends ec2.Instance {
  // return the hostname to the parent so that I can create DNS records
  // these will be required for certbot to run.
  readonly hostname: string;

  constructor(
    scope: NestedStack | Stack,
    id: string,
    props: UbuntuInstanceProps,
  ) {
    const prefix = props.domainName.split('.').shift();

    const instanceRole = new cdk.aws_iam.Role(scope, 'InstanceRole', {
      assumedBy: new cdk.aws_iam.ServicePrincipal('ec2.amazonaws.com'),
    });

    // slightly randomize the hostname so that if I need to iterate
    // on the way the ec2 instance is built, letsencrypt sees different account names
    // temporarily store this in ${h} and assign to this.hostname after super() is called.
    const h =
      props.environment === 'prod'
        ? `www${getRandomInteger(10, 99)}`
        : `${props.appSubdomain}${getRandomInteger(10, 99)}`;

    const custominit = new CustomUbuntuUserData(scope, props, h);

    // Create custom user data that installs cfn-bootstrap FIRST
    const userData = ec2.UserData.forLinux();
    userData.addCommands(
      'set -ex',
      'apt-get update',
      'apt-get install -y python3-pip',
      'pip3 install --break-system-packages https://s3.amazonaws.com/cloudformation-examples/aws-cfn-bootstrap-py3-latest.tar.gz',
      'mkdir -p /opt/aws/bin',
      'ln -sf /usr/local/bin/cfn-* /opt/aws/bin/',
    );

    // Add the CloudFormation Init commands
    const initCommand = ec2.InitCommand.argvCommand([
      '/opt/aws/bin/cfn-init',
      '-v',
      '--region',
      scope.region,
      '--stack',
      scope.stackName,
      '--resource',
      'InstanceC1063A87',
      '-c',
      'default',
    ]);
    userData.addCommands(initCommand.toString());

    const instanceSize = !props.environment.localeCompare('dev')
      ? ec2.InstanceSize.LARGE
      : ec2.InstanceSize.LARGE;

    custominit.prefix_etc_asset.grantRead(instanceRole);
    custominit.prefix_bin_asset.grantRead(instanceRole);
    custominit.ssh_keys_asset.grantRead(instanceRole);

    const ec2SecGroup = new ec2.SecurityGroup(
      scope,
      `${prefix}-${props.environment}-SecurityGroup`,
      {
        vpc: props.vpc,
      },
    );

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

    super(scope, 'Instance', {
      role: instanceRole,
      userDataCausesReplacement: true,
      userData: custominit.shellCommands,
      init: custominit.init,
      initOptions: custominit.initOptions,
      vpc: props.vpc,
      instanceType: ec2.InstanceType.of(ec2.InstanceClass.T4G, instanceSize),
      securityGroup: ec2SecGroup,
      machineImage: genericLinuxImage(scope),
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
    });

    this.hostname = h;

    console.log(`instance is at ${this.instancePublicDnsName}`);

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
            InstanceId: this.instanceId,
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
      const healthCheck = new route53.CfnHealthCheck(
        this,
        `${prefix}AppHealthCheck`,
        {
          healthCheckConfig: {
            type: 'HTTPS',
            resourcePath: '/health',
            fullyQualifiedDomainName: `${props.appSubdomain}.${props.domainName}`,
            port: 443,
            requestInterval: 30,
            failureThreshold: 3,
          },
        },
      );

      const healthAlarm = new cloudwatch.Alarm(
        this,
        `${prefix}AppHealthAlarm`,
        {
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
        },
      );
      healthAlarm.addAlarmAction(new cloudwatch_actions.SnsAction(topic));
    }
  }
}
