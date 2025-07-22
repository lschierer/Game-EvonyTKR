import { Stack, type StackProps, Duration, CfnOutput } from 'aws-cdk-lib';
import { type Construct } from 'constructs';
import * as ec2 from 'aws-cdk-lib/aws-ec2';
import * as ecs from 'aws-cdk-lib/aws-ecs';
import * as ecr from 'aws-cdk-lib/aws-ecr';
import * as elbv2 from 'aws-cdk-lib/aws-elasticloadbalancingv2';
import * as route53 from 'aws-cdk-lib/aws-route53';
import * as targets from 'aws-cdk-lib/aws-route53-targets';
import * as acm from 'aws-cdk-lib/aws-certificatemanager';
import * as logs from 'aws-cdk-lib/aws-logs';

interface MojoliciousStackProps extends StackProps {
  domainName: string;
  appSubdomain: string;
  hostedZoneId: string;
  containerPort: number;
  cpu: number;
  memory: number;
  desiredCount: number;
  ecrRepositoryName: string;
  imageTag: string;
}

export class MojoliciousStack extends Stack {
  constructor(scope: Construct, id: string, props: MojoliciousStackProps) {
    super(scope, id, props);

    // Full domain name for the application
    const fullDomainName = `${props.appSubdomain}.${props.domainName}`;

    // Import existing hosted zone
    const hostedZone = route53.HostedZone.fromHostedZoneAttributes(
      this,
      'EvonyTKRTipsHostedZone',
      {
        hostedZoneId: props.hostedZoneId,
        zoneName: props.domainName,
      },
    );

    // Create ACM certificate
    const certificate = new acm.Certificate(this, 'EvonyTKRTipsCertificate', {
      domainName: fullDomainName,
      validation: acm.CertificateValidation.fromDns(hostedZone),
    });

    // Create VPC
    const vpc = new ec2.Vpc(this, 'EvonyTKRTipsVpc', {
      maxAzs: 2,
      natGateways: 1,
    });

    // Create ECS cluster
    const cluster = new ecs.Cluster(this, 'EvonyTKRTipsCluster', {
      vpc,
      containerInsights: true,
    });

    // Import existing ECR repository (assuming you've pushed your image there)
    const repository = ecr.Repository.fromRepositoryName(
      this,
      'EvonyTKRTipsRepository',
      props.ecrRepositoryName,
    );

    // Create Fargate task definition
    const taskDefinition = new ecs.FargateTaskDefinition(
      this,
      'evonytkrtipsTaskDef',
      {
        cpu: props.cpu,
        memoryLimitMiB: props.memory,
        runtimePlatform: {
          cpuArchitecture: ecs.CpuArchitecture.ARM64,
          operatingSystemFamily: ecs.OperatingSystemFamily.LINUX,
        },
      },
    );

    // Add container to task definition
    const container = taskDefinition.addContainer('EvonyTKRTipsContainer', {
      image: ecs.ContainerImage.fromEcrRepository(repository, props.imageTag),
      logging: ecs.LogDrivers.awsLogs({
        streamPrefix: 'evonytkrtips',
        logRetention: logs.RetentionDays.ONE_WEEK,
      }),
      environment: {
        // Add any environment variables your Mojolicious app needs
        MOJO_MODE: 'production',
        MOJO_LISTEN: `http://0.0.0.0:${props.containerPort}`,
      },
    });

    // Add port mapping
    container.addPortMappings({
      containerPort: props.containerPort,
      protocol: ecs.Protocol.TCP,
    });

    // Create Fargate service
    const service = new ecs.FargateService(this, 'EvonyTKRTipsService', {
      cluster,
      taskDefinition,
      desiredCount: props.desiredCount,
      assignPublicIp: false,
      securityGroups: [
        new ec2.SecurityGroup(this, 'EvonyTKRTipsServiceSG', {
          vpc,
          description: 'Security group for evonytkrtips Fargate service',
          allowAllOutbound: true,
        }),
      ],
    });

    // Create ALB
    const lb = new elbv2.ApplicationLoadBalancer(this, 'EvonyTKRTipsALB', {
      vpc,
      internetFacing: true,
    });

    // Create HTTPS listener
    const httpsListener = lb.addListener('EvonyTKRTipsHttpsListener', {
      port: 443,
      certificates: [certificate],
      protocol: elbv2.ApplicationProtocol.HTTPS,
    });

    // Add target group to HTTPS listener
    httpsListener.addTargets('EvonyTKRTipsTargets', {
      port: props.containerPort,
      protocol: elbv2.ApplicationProtocol.HTTP,
      targets: [service],
      healthCheck: {
        path: '/health', // Adjust to your app's health check endpoint
        interval: Duration.seconds(30),
        timeout: Duration.seconds(5),
        healthyThresholdCount: 2,
        unhealthyThresholdCount: 3,
      },
    });

    // HTTP to HTTPS redirect
    const httpListener = lb.addListener('EvonyTKRTipsHttpListener', {
      port: 80,
      protocol: elbv2.ApplicationProtocol.HTTP,
      open: true,
    });

    httpListener.addAction('HttpRedirect', {
      action: elbv2.ListenerAction.redirect({
        port: '443',
        protocol: elbv2.ApplicationProtocol.HTTPS,
        permanent: true,
      }),
    });

    // Create Route53 record
    new route53.ARecord(this, 'EvonyTKRTipsDNSRecord', {
      zone: hostedZone,
      recordName: props.appSubdomain,
      target: route53.RecordTarget.fromAlias(
        new targets.LoadBalancerTarget(lb),
      ),
      ttl: Duration.minutes(5),
    });

    // Outputs
    new CfnOutput(this, 'EvonyTKRTipsLoadBalancerDNS', {
      value: lb.loadBalancerDnsName,
      description: 'Load Balancer DNS Name',
    });

    new CfnOutput(this, 'EvonyTKRTipsApplicationURL', {
      value: `https://${fullDomainName}`,
      description: 'Application URL',
    });
  }
}
