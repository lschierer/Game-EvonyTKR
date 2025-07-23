import { App } from 'aws-cdk-lib';
import { MojoliciousStack } from '../lib/infra-stack.ts';

const app = new App();

type EnvConfig = {
  subdomain: string;
  desiredCount: number;
  cpu: number;
  memory: number;
  imageTag: string;
};

const envConfigs: Record<string, EnvConfig> = {
  dev: {
    subdomain: 'dev',
    desiredCount: 1,
    cpu: 1024,
    memory: 4096,
    imageTag: 'latest',
  },
  prod: {
    subdomain: 'www',
    desiredCount: 2,
    cpu: 2048,
    memory: 5120,
    imageTag: 'stable',
  },
};

const environment: string = (app.node.tryGetContext('env') as string) || 'dev';
const config = envConfigs[environment];

new MojoliciousStack(app, `evonytkrtips-${environment}-stack`, {
  domainName: 'evonytkrtips.net', // Your Route53 domain
  appSubdomain: config.subdomain,
  hostedZoneId: 'Z02705452UES0AYN9485J', // Your Route53 hosted zone ID
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
  tags: {
    Environment: environment,
    Application: 'EvonyTKRTips',
  },
});
