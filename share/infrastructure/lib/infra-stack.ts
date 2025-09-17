#!/usr/bin/env node
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
import * as s3 from 'aws-cdk-lib/aws-s3';
import * as cdk from 'aws-cdk-lib';
import * as iam from 'aws-cdk-lib/aws-iam';
import {
  aws_events as events,
  aws_events_targets as etargets,
  aws_lambda as lambda,
} from 'aws-cdk-lib';

interface MojoliciousStackProps extends StackProps {
  environment: string;
  CidrRange: string;
  domainName: string;
  appSubdomain: string;
  hostedZoneId: string;
  zoneName: string;
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

    const logbucket = new s3.Bucket(this, 'EvonyTKRTipsLogBucket', {
      encryption: s3.BucketEncryption.KMS_MANAGED,
      blockPublicAccess: s3.BlockPublicAccess.BLOCK_ALL,
      objectOwnership: s3.ObjectOwnership.BUCKET_OWNER_ENFORCED,
      removalPolicy: cdk.RemovalPolicy.DESTROY,
      autoDeleteObjects: true,
      enforceSSL: true,
      minimumTLSVersion: 1.2,
      intelligentTieringConfigurations: [
        {
          name: 'EvonyTKRTipsLogs',
          prefix: '*',
          archiveAccessTierTime: Duration.days(90),
          deepArchiveAccessTierTime: Duration.days(180),
        },
      ],
    });

    const syncTaskRole = new iam.Role(this, 'LogSyncTaskRole', {
      assumedBy: new iam.ServicePrincipal('ecs-tasks.amazonaws.com'),
    });

    logbucket.grantWrite(syncTaskRole);

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
    const certificate = new acm.Certificate(this, 'UnifiedCert', {
      domainName: `${props.appSubdomain}.${props.domainName}`,
      subjectAlternativeNames:
        props.environment === 'prod' ? ['evonytkrtips.net'] : [],
      validation: acm.CertificateValidation.fromDns(hostedZone),
    });

    const vpc = new ec2.Vpc(this, 'EvonyTKRTipsVpc', {
      ipAddresses: ec2.IpAddresses.cidr(props.CidrRange),
      maxAzs: 2,
      natGateways: 0,
      subnetConfiguration: [
        {
          name: 'public',
          subnetType: ec2.SubnetType.PUBLIC,
          cidrMask: 28,
        },
      ],
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
        taskRole: syncTaskRole,
      },
    );

    // Add a volume to share logs
    taskDefinition.addVolume({
      name: 'perl-logs',
      host: {}, // Empty host means it's an ephemeral volume
    });

    // Add container to task definition
    const appContainer = taskDefinition.addContainer('EvonyTKRTipsContainer', {
      image: ecs.ContainerImage.fromEcrRepository(repository, props.imageTag),
      logging: ecs.LogDrivers.awsLogs({
        streamPrefix: 'evonytkrtips',
        logRetention: logs.RetentionDays.ONE_WEEK,
      }),
      environment: {
        // Add any environment variables your Mojolicious app needs
        MOJO_MODE: 'production',
        MOJO_LISTEN: `http://0.0.0.0:${props.containerPort}`,
        HOME: '/home/mojo',
        IMAGE_TAG: props.imageTag,
        IMAGE_URI: `${repository.repositoryUri}:${props.imageTag}`,
        DEPLOYMENT_TIME: new Date().toISOString(),
      },
      healthCheck: {
        command: [
          'CMD-SHELL',
          `curl -f http://localhost:${props.containerPort}/health || exit 1`,
        ],
        interval: Duration.seconds(30),
        timeout: Duration.seconds(5),
        retries: 3,
        startPeriod: Duration.seconds(240), // wait this long before running the first check
      },
    });

    // Add port mapping
    appContainer.addPortMappings({
      containerPort: props.containerPort,
      protocol: ecs.Protocol.TCP,
    });

    // Mount the logs volume in your app container
    appContainer.addMountPoints({
      containerPath: '/home/mojo/var/log/Perl/dist/Game-Evony',
      sourceVolume: 'perl-logs',
      readOnly: false,
    });

    const albSG = new ec2.SecurityGroup(this, 'EvonyTKRTipsAlbSG', {
      vpc,
      description: 'ALB SG',
      allowAllOutbound: true,
    });
    albSG.addIngressRule(ec2.Peer.anyIpv4(), ec2.Port.tcp(443));
    albSG.addIngressRule(ec2.Peer.anyIpv4(), ec2.Port.tcp(80));

    const serviceSG = new ec2.SecurityGroup(this, 'EvonyTKRTipsServiceSG', {
      vpc,
      description: 'Fargate service SG',
      allowAllOutbound: true,
    });
    serviceSG.addIngressRule(albSG, ec2.Port.tcp(props.containerPort));

    // Create Fargate service
    const service = new ecs.FargateService(this, 'EvonyTKRTipsService', {
      cluster,
      taskDefinition,
      desiredCount: props.desiredCount,
      enableExecuteCommand: true,
      assignPublicIp: true,
      healthCheckGracePeriod: Duration.seconds(300),
      securityGroups: [albSG, serviceSG],
    });

    // Create ALB
    const lb = new elbv2.ApplicationLoadBalancer(this, 'EvonyTKRTipsALB', {
      vpc,
      internetFacing: true,
    });

    lb.addSecurityGroup(albSG);

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
    if (props.environment === 'prod') {
      new route53.ARecord(this, 'RootDomainRecord', {
        zone: hostedZone,
        recordName: '', // root domain
        target: route53.RecordTarget.fromAlias(
          new targets.LoadBalancerTarget(lb),
        ),
      });
    }

    new route53.ARecord(this, 'EvonyTKRTipsDNSRecord', {
      zone: hostedZone,
      recordName: props.appSubdomain,
      target: route53.RecordTarget.fromAlias(
        new targets.LoadBalancerTarget(lb),
      ),
      ttl: Duration.minutes(5),
    });

    // Add sidecar container for log shipping to S3
    const logShipperContainer = taskDefinition.addContainer(
      'LogShipperContainer',
      {
        image: ecs.ContainerImage.fromRegistry(
          'amazon/aws-for-fluent-bit:stable',
        ),
        essential: false,
        cpu: 128,
        memoryReservationMiB: 128,
        logging: ecs.LogDrivers.awsLogs({
          streamPrefix: 'log-shipper',
          logRetention: logs.RetentionDays.ONE_WEEK,
        }),
        environment: {
          AWS_REGION: this.region,
          S3_BUCKET: logbucket.bucketName,
          // Enable ECS metadata endpoint v4
          ECS_ENABLE_CONTAINER_METADATA: 'true',
        },
        // Use only command line configuration, no config file
        command: [
          '/fluent-bit/bin/fluent-bit',
          '-i',
          'tail',
          '-p',
          'path=/var/log/app/*.log',
          '-p',
          'tag=app',
          '-p',
          'refresh_interval=5',
          '-p',
          'path_key=filepath',
          '-o',
          's3',
          '-p',
          'match=*',
          '-p',
          'bucket=${S3_BUCKET}',
          '-p',
          'region=${AWS_REGION}',
          '-p',
          's3_key_format=/logs/%Y/%m/%d/${filename}-%H%M%S-${HOSTNAME}.log',
          '-p',
          'total_file_size=10M',
          '-p',
          'upload_timeout=10m',
          '-p',
          'use_put_object=On',
          '-p',
          'compression=gzip',
          '-v',
        ],
      },
    );

    // Mount the logs volume in the log shipper container
    logShipperContainer.addMountPoints({
      containerPath: '/var/log/app',
      sourceVolume: 'perl-logs',
      readOnly: true,
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

    if (props.environment == 'dev') {
      this.addSelfDestruct(24);
    }
  }

  private addSelfDestruct(hours: number) {
    const fn = new lambda.Function(this, 'SelfDestructFn', {
      runtime: lambda.Runtime.NODEJS_LATEST,
      handler: 'index.handler',
      code: lambda.Code.fromInline(`
        const AWS = require('aws-sdk');
        const cf = new AWS.CloudFormation();
        exports.handler = async () => {
          const stackName = process.env.STACK_NAME;
          const maxAgeMs = Number(process.env.MAX_AGE_MS);
          const d = await cf.describeStacks({ StackName: stackName }).promise();
          const created = new Date(d.Stacks[0].CreationTime).getTime();
          const age = Date.now() - created;
          console.log({stackName, created, age, maxAgeMs});
          if (age >= maxAgeMs) {
            console.log('Deleting stack…');
            await cf.deleteStack({ StackName: stackName }).promise();
          } else {
            console.log('Not old enough yet.');
          }
        };
      `),
      timeout: Duration.minutes(5),
      environment: {
        STACK_NAME: Stack.of(this).stackName,
        MAX_AGE_MS: String(hours * 60 * 60 * 1000),
      },
    });

    // allow the function to look up and delete THIS stack only
    Stack.of(this).stackId.replace(':stack/', ':stack/*'); // describe needs wildcards sometimes
    fn.addToRolePolicy(
      new iam.PolicyStatement({
        actions: ['cloudformation:DescribeStacks'],
        resources: ['*'], // DescribeStacks doesn’t support resource-level perms reliably
      }),
    );
    fn.addToRolePolicy(
      new iam.PolicyStatement({
        actions: ['cloudformation:DeleteStack'],
        resources: [Stack.of(this).stackId],
      }),
    );

    // run every 3 hours; first run will be <24h so it will skip, then delete once it ages out
    const rule = new events.Rule(this, 'SelfDestructRule', {
      schedule: events.Schedule.rate(Duration.hours(3)),
    });
    rule.addTarget(new etargets.LambdaFunction(fn));
  }
}
