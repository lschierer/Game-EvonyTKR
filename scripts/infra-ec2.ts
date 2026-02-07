#!/usr/bin/env node
import * as cdk from "aws-cdk-lib/core";
import * as ec2 from 'aws-cdk-lib/aws-ec2';
import path from 'path';
import { fileURLToPath } from 'url';

// Import from common framework
import {
  ApplicationStack,
  type ApplicationStackProps,
} from '../../PAGI-WebServer/lib/Infrastructure/index.ts';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = new cdk.App();

import { execSync } from "child_process";

try {
  const stdout = execSync(
    "podman machine inspect --format 'unix://{{.ConnectionInfo.PodmanSocket.Path}}'",
    { encoding: "utf-8" },
  ).trim();

  process.env.DOCKER_HOST = stdout;
  process.env.CDK_DOCKER = "podman";
} catch (error) {
  console.error(`Failed to get podman socket: ${error as string}`);
  process.exit(1);
}

const environment = app.node.tryGetContext('env') || 'dev';
const region = process.env.REGION ?? "us-east-2";

type EnvConfig = {
  CidrRange: string;
  subdomain: string;
  instanceSize: ec2.InstanceSize;
};

const envConfigs: Record<string, EnvConfig> = {
  dev: {
    CidrRange: '10.193.0.0/27',
    subdomain: 'dev',
    instanceSize: ec2.InstanceSize.SMALL,
  },
  prod: {
    CidrRange: '10.199.0.0/27',
    subdomain: 'www',
    instanceSize: ec2.InstanceSize.MEDIUM,
  },
};

const config = envConfigs[environment];

const props: ApplicationStackProps = {
  env: {
    account: process.env.CDK_DEFAULT_ACCOUNT,
    region,
  },
  mode: environment === 'prod' ? 'prod' : 'dev',
  CidrRange: config.CidrRange,
  prefix: "EvonyTKR",
  appSubdomain: config.subdomain,
  domainName: "evonytkrtips.net",
  hostedZoneId: "Z02705452UES0AYN9485J",
  zoneName: 'evonytkrtips.net',
  instanceSize: config.instanceSize,
  appPort: 3000,
  mainPerlDistro: 'Game-EvonyTKR',
  appCodePath: path.join(__dirname, '..'),
  appCodeExcludes: [
    'node_modules',
    'share/infrastructure/node_modules',
    'share/infrastructure/cdk.out',
    '.git',
    'dist',
  ],
  crossRegionReferences: true,
  tags: {
    Environment: environment,
    Application: 'EvonyTKRTips',
  },
};

new ApplicationStack(app, `evonytkrtips-${environment}-ec2-stack`, props);

app.synth();
