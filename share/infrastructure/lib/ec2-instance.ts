import {
  NestedStack,
  type NestedStackProps,
  Duration,
  CfnOutput,
} from 'aws-cdk-lib';
import { type Construct } from 'constructs';
import * as ec2 from 'aws-cdk-lib/aws-ec2';

import { type MojoliciousStackProps } from './main-stack';

interface UbuntuInstanceProps extends MojoliciousStackProps {
  vpc: ec2.IVpc | ec2.Vpc;
}

export class UbuntuInstance extends NestedStack {
  readonly instance;
  constructor(scope: Construct, id: string, props: UbuntuInstanceProps) {
    super(scope, id);

    const instanceSize = !props.environment.localeCompare('dev')
      ? ec2.InstanceSize.SMALL
      : ec2.InstanceSize.MEDIUM;

    const ec2SecGroup = new ec2.SecurityGroup(
      this,
      `Mojo-${props.environment}-SecurityGroup`,
      {
        vpc: props.vpc,
      },
    );

    this.instance = new ec2.Instance(this, 'Instance', {
      vpc: props.vpc,
      instanceType: ec2.InstanceType.of(
        ec2.InstanceClass.BURSTABLE3,
        instanceSize,
      ),
      securityGroup: ec2SecGroup,
      machineImage: this.genericLinuxImage(),
      vpcSubnets: { subnetType: ec2.SubnetType.PUBLIC },
    });

    const ubuntuInstance = new CfnOutput(
      this,
      `Mojo-${props.environment}-PublicIP`,
      {
        value: this.instance.instancePublicIp,
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
  }

  genericLinuxImage() {
    const ubuntuCompanyOwnerId = '099720109477';
    // NOTE only pick LTS versions for your sanity!
    const ubuntuName = 'noble';

    const machineImage = ec2.MachineImage.genericLinux({
      [this.region]: new ec2.LookupMachineImage({
        // `YEAR-ARCH` are the first two stars
        name: `ubuntu/images/hvm-ssd/ubuntu-${ubuntuName}-*-*-server-*`,
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
}
