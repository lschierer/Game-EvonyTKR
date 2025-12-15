import { Stack, type StackProps, Duration, RemovalPolicy } from 'aws-cdk-lib';
import { type Construct } from 'constructs';
import * as ec2 from 'aws-cdk-lib/aws-ec2';
import * as dynamodb from 'aws-cdk-lib/aws-dynamodb';
import * as route53 from 'aws-cdk-lib/aws-route53';

import * as iam from 'aws-cdk-lib/aws-iam';
import {
  aws_events as events,
  aws_events_targets as etargets,
  aws_lambda as lambda,
} from 'aws-cdk-lib';

import { UbuntuInstance } from './ec2-instance';

export interface MojoliciousStackProps extends StackProps {
  environment: string;
  CidrRange: string;
  domainName: string;
  appSubdomain: string;
  hostedZoneId: string;
  zoneName: string;
  mojoPort: number;
  cpu: number;
  memory: number;
  desiredCount: number;
}

export class MojoliciousStack extends Stack {
  readonly applicationURL;
  constructor(scope: Construct, id: string, props: MojoliciousStackProps) {
    super(scope, id, props);

    this.applicationURL = `${props.appSubdomain}.${props.domainName}`;

    // Import existing hosted zone
    const hostedZone = route53.HostedZone.fromHostedZoneAttributes(
      this,
      'EvonyTKRTipsHostedZone',
      {
        hostedZoneId: props.hostedZoneId,
        zoneName: props.domainName,
      },
    );

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

    // Create DynamoDB table for persistence (production only)
    // Development uses SQLite for everything
    const persistenceTable = new dynamodb.Table(this, 'PersistenceTable', {
      tableName: `evonytkr-${props.environment}-data`,
      partitionKey: { name: 'pk', type: dynamodb.AttributeType.STRING },
      sortKey: { name: 'sk', type: dynamodb.AttributeType.STRING },
      billingMode: dynamodb.BillingMode.PAY_PER_REQUEST, // On-demand pricing for cost efficiency
      pointInTimeRecovery: props.environment === 'prod', // Enable PITR for production only
      removalPolicy: props.environment === 'prod'
        ? RemovalPolicy.SNAPSHOT  // Snapshot prod data on stack deletion
        : RemovalPolicy.DESTROY,   // Clean up dev data automatically
      encryption: dynamodb.TableEncryption.AWS_MANAGED, // Use AWS-managed encryption
    });

    // Create DynamoDB table for work unit completion tracking
    const workUnitsTable = new dynamodb.Table(this, 'WorkUnitsTable', {
      tableName: `evony-work-units`,
      partitionKey: { name: 'work_unit_id', type: dynamodb.AttributeType.STRING },
      billingMode: dynamodb.BillingMode.PAY_PER_REQUEST,
      removalPolicy: RemovalPolicy.DESTROY, // Always clean up work units
      encryption: dynamodb.TableEncryption.AWS_MANAGED,
    });

    const InstanceStack = new UbuntuInstance(
      this,
      `Mojo-${props.environment}-instance`,
      {
        ...props,
        vpc,
        persistenceTable,
        workUnitsTable,
      },
    );

    // Create Route53 record
    if (props.environment === 'prod') {
      new route53.ARecord(this, 'RootDomainRecord', {
        zone: hostedZone,
        recordName: '', // root domain
        target: route53.RecordTarget.fromIpAddresses(
          InstanceStack.instance.instancePublicIp,
        ),
      });
    }

    new route53.ARecord(this, 'EvonyTKRTipsDNSRecord', {
      zone: hostedZone,
      recordName: `${props.appSubdomain}2`,
      target: route53.RecordTarget.fromIpAddresses(
        InstanceStack.instance.instancePublicIp,
      ),
      ttl: Duration.minutes(5),
    });

    new route53.ARecord(this, 'EvonyTKRTipsWWWDNSRecord', {
      zone: hostedZone,
      recordName: `www.${props.appSubdomain}2`,
      target: route53.RecordTarget.fromIpAddresses(
        InstanceStack.instance.instancePublicIp,
      ),
      ttl: Duration.minutes(5),
    });

    new route53.ARecord(this, 'InstanceDNSRecord', {
      zone: hostedZone,
      recordName: `${InstanceStack.hostname}`,
      target: route53.RecordTarget.fromIpAddresses(
        InstanceStack.instance.instancePublicIp,
      ),
      ttl: Duration.minutes(5),
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
