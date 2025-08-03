import { Stack, type StackProps } from 'aws-cdk-lib';
import { type Construct } from 'constructs';
import * as acm from 'aws-cdk-lib/aws-certificatemanager';
import * as cloudfront from 'aws-cdk-lib/aws-cloudfront';
import * as origins from 'aws-cdk-lib/aws-cloudfront-origins';
import * as s3 from 'aws-cdk-lib/aws-s3';
import * as route53 from 'aws-cdk-lib/aws-route53';
import * as targets from 'aws-cdk-lib/aws-route53-targets';
import { HostedZone } from 'aws-cdk-lib/aws-route53';

interface RootRedirectStackProps extends StackProps {
  hostedZoneId: string;
  zoneName: string;
}

export class RootRedirectStack extends Stack {
  constructor(scope: Construct, id: string, props: RootRedirectStackProps) {
    super(scope, id, {
      ...props,
      env: { region: 'us-east-1' },
      crossRegionReferences: true,
    });

    const hostedZone = HostedZone.fromHostedZoneAttributes(this, 'HostedZone', {
      hostedZoneId: props.hostedZoneId,
      zoneName: props.zoneName,
    });

    const bucket = new s3.Bucket(this, 'RedirectBucket', {
      websiteRedirect: {
        hostName: 'www.evonytkrtips.net',
        protocol: s3.RedirectProtocol.HTTPS,
      },
    });

    const cert = new acm.Certificate(this, 'RootCert', {
      domainName: 'evonytkrtips.net',
      validation: acm.CertificateValidation.fromDns(hostedZone),
    });

    const dist = new cloudfront.Distribution(this, 'RedirectDistribution', {
      defaultBehavior: {
        origin: origins.S3BucketOrigin.withOriginAccessControl(bucket),
        viewerProtocolPolicy: cloudfront.ViewerProtocolPolicy.REDIRECT_TO_HTTPS,
      },
      certificate: cert,
      domainNames: ['evonytkrtips.net'],
    });

    new route53.ARecord(this, 'RootAlias', {
      zone: hostedZone,
      recordName: '',
      target: route53.RecordTarget.fromAlias(
        new targets.CloudFrontTarget(dist),
      ),
    });
  }
}
