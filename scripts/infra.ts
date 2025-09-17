import { App } from 'aws-cdk-lib';

import { MojoliciousStack } from '../share/infrastructure/lib/infra-stack.ts';

const app = new App();

type EnvConfig = {
  CidrRange: string;
  subdomain: string;
  desiredCount: number;
  cpu: number;
  memory: number;
  imageTag: string;
};

const envConfigs: Record<string, EnvConfig> = {
  dev: {
    CidrRange: '10.193.0.0/24',
    subdomain: 'dev',
    desiredCount: 1,
    cpu: 2048,
    memory: 4096,
    imageTag: 'latest',
  },
  prod: {
    CidrRange: '10.199.0.0/24',
    subdomain: 'www',
    desiredCount: 2,
    cpu: 4096,
    memory: 6144,
    imageTag: 'stable',
  },
};

const environment: string = (app.node.tryGetContext('env') as string) || 'dev';
const config = envConfigs[environment];
const hostedZoneId = 'Z02705452UES0AYN9485J'; // Your Route53 hosted zone ID
const zoneName = 'evonytkrtips.net';

new MojoliciousStack(app, `evonytkrtips-${environment}-stack`, {
  environment: environment,
  CidrRange: config.CidrRange,
  domainName: 'evonytkrtips.net', // Your Route53 domain
  appSubdomain: config.subdomain,
  hostedZoneId: hostedZoneId, // Your Route53 hosted zone ID
  zoneName: zoneName,
  containerPort: 3000, // Port your Mojolicious app listens on
  cpu: config.cpu,
  memory: config.memory,
  desiredCount: config.desiredCount,
  ecrRepositoryName: 'evonytkrtips', // ECR repository name
  imageTag: config.imageTag,
  env: {
    account: '699040795025',
    region: 'us-east-2',
  },
  crossRegionReferences: true,
  tags: {
    Environment: environment,
    Application: 'EvonyTKRTips',
  },
});
