#!/usr/bin/env node
import * as cdk from "aws-cdk-lib/core";
import * as ec2 from 'aws-cdk-lib/aws-ec2';
import path from 'path';
import { fileURLToPath } from 'url';

// Import from common framework
import {
  ApplicationStack,
  type ApplicationStackProps,
} from '../../../PAGI-WebServer/lib/Infrastructure/index.ts';

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

const mode = process.env.MODE ?? 'dev';
const region = process.env.REGION ?? "us-east-2";

let props: ApplicationStackProps;
if(!process.env.MODE?.localeCompare('prod')){
  props = {
    env: {
      account: process.env.CDK_DEFAULT_ACCOUNT,
      region,
    },
    mode: 'prod',
    prefix: "EvonyTKR",
    appSubdomain: 'www',
    domainName: "evonytkrtips.net",
    hostedZoneId: "Z02705452UES0AYN9485J",
    zoneName: 'evonytkrtips.net',
    instanceSize: ec2.InstanceSize.LARGE,
    appPort: 3000,
    mainPerlDistro: 'Game-EvonyTKR',
    appCodePath: path.join(__dirname, '../..'),
    appCodeExcludes: [
      'node_modules',
      'infrastructure/node_modules',
      'infrastructure/cdk.out',
      'infrastructure',
      '.git',
      'dist',
    ],
  };
  
} else if (!mode.localeCompare('dev')){
  props = {
    env: {
      account: process.env.CDK_DEFAULT_ACCOUNT,
      region,
    },
    prefix: "EvonyTKR",
    mode: 'dev',
    appSubdomain: 'dev',
    domainName: "evonytkrtips.net",
    hostedZoneId: "Z02705452UES0AYN9485J",
    zoneName: 'evonytkrtips.net',
    instanceSize: ec2.InstanceSize.LARGE,
    appPort: 3000,
    mainPerlDistro: 'Game-EvonyTKR',
    appCodePath: path.join(__dirname, '../..'),
    appCodeExcludes: [
      'node_modules',
      'infrastructure/node_modules',
      'infrastructure/cdk.out',
      '.git',
      'dist',
    ],
  };
} else {
  props = {
    env: {
      account: process.env.CDK_DEFAULT_ACCOUNT,
      region,
    },
    prefix: "EvonyTKR",
    mode: 'test',
    appSubdomain: mode,
    domainName: "evonytkrtips.net",
    hostedZoneId: "Z02705452UES0AYN9485J",
    zoneName: 'evonytkrtips.net',
    instanceSize: ec2.InstanceSize.LARGE,
    appPort: 3000,
    mainPerlDistro: 'Game-EvonyTKR',
    appCodePath: path.join(__dirname, '../..'),
    appCodeExcludes: [
      'node_modules',
      'infrastructure/node_modules',
      'infrastructure/cdk.out',
      '.git',
      'dist',
    ],
  };
}

new ApplicationStack(app, `EvonyTKR-${mode}`, props);
