import * as ec2 from 'aws-cdk-lib/aws-ec2';
//import * as iam from 'aws-cdk-lib/aws-iam';
import * as s3Assets from 'aws-cdk-lib/aws-s3-assets';

import path from 'path';
import { fileURLToPath } from 'url';
import * as cdk from 'aws-cdk-lib';
import { Stack } from 'aws-cdk-lib';

import { type UbuntuInstanceProps } from './ec2-instance';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export class CustomUbuntuUserData {
  public init: ec2.CloudFormationInit;

  public initOptions: ec2.ApplyCloudFormationInitOptions;

  public prefix_bin_asset: s3Assets.Asset;

  public prefix_etc_asset: s3Assets.Asset;

  public ssh_keys_asset: s3Assets.Asset;

  public shellCommands: ec2.UserData;

  public constructor(stack: Stack, props: UbuntuInstanceProps) {
    const hostname =
      props.environment === 'prod'
        ? `www${this.getRandomInteger(10, 99)}`
        : `${props.appSubdomain}${this.getRandomInteger(10, 99)}`;

    this.prefix_bin_asset = new s3Assets.Asset(stack, 'prefixBinAsset', {
      path: path.join(__dirname, '../prefixBin'),
    });

    this.prefix_etc_asset = new s3Assets.Asset(stack, 'prefix_etc_asset', {
      path: path.join(__dirname, '../etc'),
    });

    this.ssh_keys_asset = new s3Assets.Asset(stack, 'sshKeysAsset', {
      path: path.join(__dirname, '../ssh_keys/authorized_keys'),
    });

    this.shellCommands = ec2.UserData.forLinux();
    this.shellCommands.addCommands(
      'set -ex',
      'apt-get update',
      'apt-get install -y python3-pip unzip curl',
      'pip3 install --break-system-packages https://s3.amazonaws.com/cloudformation-examples/aws-cfn-bootstrap-py3-latest.tar.gz',
      'mkdir -p /opt/aws/bin',
      'ln -sf /usr/local/bin/cfn-* /opt/aws/bin/',
      'curl "https://awscli.amazonaws.com/awscli-exe-linux-aarch64.zip" -o "awscliv2.zip"',
      'unzip awscliv2.zip',
      './aws/install',
    );

    const local_bin_path = this.shellCommands.addS3DownloadCommand({
      bucket: this.prefix_bin_asset.bucket,
      bucketKey: this.prefix_bin_asset.s3ObjectKey,
    });

    const local_etc_path = this.shellCommands.addS3DownloadCommand({
      bucket: this.prefix_etc_asset.bucket,
      bucketKey: this.prefix_etc_asset.s3ObjectKey,
    });

    const local_ssh_keys_path = this.shellCommands.addS3DownloadCommand({
      bucket: this.ssh_keys_asset.bucket,
      bucketKey: this.ssh_keys_asset.s3ObjectKey,
    });

    const hostprefix =
      props.environment === 'prod' ? 'production' : props.environment;

    this.init = ec2.CloudFormationInit.fromConfigSets({
      configSets: {
        default: [
          'installPackages',
          'createUsers',
          'configureSSM',
          'configureApp',
        ],
      },
      configs: {
        installPackages: new ec2.InitConfig([
          ec2.InitCommand.shellCommand('add-apt-repository -y universe'),
          ec2.InitCommand.shellCommand('apt-get update'),
          ec2.InitCommand.shellCommand('apt-get dist-upgrade -y'),
          ec2.InitPackage.apt('autoconf'),
          ec2.InitPackage.apt('autoconf-archive'),
          ec2.InitPackage.apt('automake'),
          ec2.InitPackage.apt('bison'),
          ec2.InitPackage.apt('build-essential'),
          ec2.InitPackage.apt('certbot'),
          ec2.InitPackage.apt('curl'),
          ec2.InitPackage.apt('flex'),
          ec2.InitPackage.apt('gcc'),
          ec2.InitPackage.apt('gcc-13-locales'),
          ec2.InitPackage.apt('gettext'),
          ec2.InitPackage.apt('git'),
          ec2.InitPackage.apt('gnu-standards'),
          ec2.InitPackage.apt('graphviz'),
          ec2.InitPackage.apt('libgd-dev'),
          ec2.InitPackage.apt('libgraphviz-dev'),
          ec2.InitPackage.apt('libssl-dev'),
          ec2.InitPackage.apt('libtool'),
          ec2.InitPackage.apt('libzip-dev'),
          ec2.InitPackage.apt('make'),
          ec2.InitPackage.apt('nginx'),
          ec2.InitPackage.apt('openssh-server'),
          ec2.InitPackage.apt('pipx'),
          ec2.InitPackage.apt('pkg-config'),
          ec2.InitPackage.apt('plocate'),
          ec2.InitPackage.apt('postgresql'),
          ec2.InitPackage.apt('postgresql-contrib'),
          ec2.InitPackage.apt('postgresql-server-dev-16'),
          ec2.InitPackage.apt('python3'),
          ec2.InitPackage.apt('python3-certbot-nginx'),
          ec2.InitPackage.apt('python3-pip'),
          ec2.InitPackage.apt('rsync'),
          ec2.InitPackage.apt('sqlite3'),
          ec2.InitPackage.apt('sysstat'),
          ec2.InitPackage.apt('unattended-upgrades'),
          ec2.InitPackage.apt('unzip'),
          ec2.InitPackage.apt('update-notifier-common'),
          ec2.InitCommand.shellCommand('systemctl is-active sysstat'),
          ec2.InitCommand.shellCommand('systemctl enable unattended-upgrades'),
        ]),
        createUsers: new ec2.InitConfig([
          ec2.InitUser.fromName('appuser', {
            homeDir: '/opt/prefix',
            groups: ['www-data'],
          }),
          ec2.InitCommand.shellCommand(
            '/usr/bin/chsh -s /sbin/nologin appuser',
          ),
          ec2.InitUser.fromName('luke', {
            groups: [
              'users',
              'admin',
              'adm',
              'man',
              'sudo',
              'operator',
              'src',
              'staff',
              'crontab',
              'www-data',
            ],
          }),
          // TODO: set up sudo for 'luke' user.
          ec2.InitCommand.shellCommand(
            ' /usr/sbin/groupmod -a appuser -U luke',
          ),
          ec2.InitCommand.shellCommand('mkdir -p /home/luke/.ssh'),
          ec2.InitCommand.shellCommand('chmod 700 /home/luke/.ssh'),
          ec2.InitCommand.shellCommand(
            `cp ${local_ssh_keys_path} /home/luke/.ssh/authorized_keys`,
          ),
          ec2.InitCommand.shellCommand(
            'chmod 600 /home/luke/.ssh/authorized_keys',
          ),
          ec2.InitCommand.shellCommand('chown -R luke:luke /home/luke/.ssh'),
        ]),
        configureSSM: new ec2.InitConfig([
          ec2.InitCommand.shellCommand('apt-get remove --purge -y snapd'),
          ec2.InitCommand.shellCommand(
            'curl "https://s3.amazonaws.com/ec2-downloads-windows/SSMAgent/latest/debian_arm64/amazon-ssm-agent.deb" -o "/tmp/amazon-ssm-agent.deb"',
          ),
          ec2.InitCommand.shellCommand('dpkg -i /tmp/amazon-ssm-agent.deb'),
          ec2.InitCommand.shellCommand('systemctl enable amazon-ssm-agent'),
          ec2.InitCommand.shellCommand('systemctl start amazon-ssm-agent'),
        ]),
        configureApp: new ec2.InitConfig([
          ec2.InitFile.fromObject('/etc/stack.json', {
            stackId: Stack.of(stack).stackId,
            stackName: Stack.of(stack).stackName,
            region: Stack.of(stack).region,
          }),
          ec2.InitCommand.shellCommand(`hostname ${hostname}`),
          ec2.InitCommand.shellCommand(`hostnamectl set-hostname ${hostname}`),
          ec2.InitCommand.shellCommand(
            `echo "127.0.1.1 ${hostname} ${hostname}.localdoamin" >> /etc/hosts`,
          ),
          ec2.InitCommand.shellCommand('mkdir -p /opt/prefix/var/run'),
          ec2.InitCommand.shellCommand('mkdir -p /opt/prefix/var/log'),
          ec2.InitCommand.shellCommand('mkdir -p /opt/prefix/bin'),
          ec2.InitCommand.shellCommand('mkdir -p /opt/prefix/etc'),
          ec2.InitCommand.shellCommand('chown -R appuser:www-data /opt/prefix'),
          ec2.InitCommand.shellCommand(
            'sudo -u appuser touch /opt/prefix/.bash_profile',
          ),
          ec2.InitCommand.shellCommand(
            `echo 'export PATH="/opt/prefix/.local/bin/:$HOME/bin:$PATH"' >> /opt/prefix/.bash_profile`,
          ),
          ec2.InitCommand.shellCommand('mkdir /etc/nginx/ssl'),
          ec2.InitCommand.shellCommand(
            'openssl req -x509 -newkey rsa:2048 -keyout /etc/nginx/ssl/evonytkrtips.net.key -out /etc/nginx/ssl/evonytkrtips.net.crt -days 365 -nodes -subj "/C=US/ST=State/L=City/O=Org/OU=OrgUnit/CN=localhost"',
          ),
          // if there is not a cert present where the nginx config states, then
          // when certbot runs, it checks the nginx config syntax for correctness and it will fail
          ec2.InitCommand.shellCommand(
            'chown -R www-data:www-data /etc/nginx/ssl',
          ),
          ec2.InitCommand.shellCommand(
            'sync && echo 3 > /proc/sys/vm/drop_caches',
          ),
          ec2.InitCommand.shellCommand(
            'mkdir -p /opt/prefix/var/log/Perl/dist/Game-EvonyTKR/',
          ),
          ec2.InitCommand.shellCommand(
            'mkdir -p /home/luke/var/log/Perl/dist/Game-EvonyTKR/',
          ),
          ec2.InitCommand.shellCommand(
            'chown -R appuser:www-data /opt/prefix/var',
          ),
          ec2.InitCommand.shellCommand('chown -R luke:luke /home/luke/var'),
          ec2.InitCommand.shellCommand(
            'find /opt/prefix/var -type d -exec chmod g+rx {} \\;',
          ),
          ec2.InitCommand.shellCommand(
            `cd /opt/prefix/bin && unzip ${local_bin_path}`,
          ),
          ec2.InitCommand.shellCommand('chown -R appuser:www-data /opt/prefix'),
          ec2.InitCommand.shellCommand('chmod +x /opt/prefix/bin/*.sh'),
          ec2.InitCommand.shellCommand(
            'mv /opt/prefix/bin/.bash* /opt/prefix/',
          ),

          ec2.InitCommand.shellCommand(
            'sudo -u appuser -s /bin/bash -l -c /opt/prefix/bin/bootstrap.sh',
          ),
          ec2.InitCommand.shellCommand(
            'echo "BOOTSTRAP_EXIT_CODE=$?" | sudo tee /var/log/cloud-output-init.log',
          ),

          ec2.InitCommand.shellCommand(
            'cp /opt/prefix/bin/deploy-prefix.sh /usr/local/bin',
          ),
          ec2.InitCommand.shellCommand(
            'chmod 0755 /opt/prefix/bin/deploy-prefix.sh',
          ),

          ec2.InitCommand.shellCommand(
            `sed -i -E 's/REPLACE2/${hostprefix}/' /opt/prefix/bin/setup-cert.sh`,
          ),
          ec2.InitCommand.shellCommand(
            'cp /opt/prefix/bin/setup-cert.sh /usr/local/bin',
          ),
          ec2.InitCommand.shellCommand(
            'chmod 0755 /usr/local/bin/setup-cert.sh',
          ),
          ec2.InitCommand.shellCommand('mkdir -p /tmp/prefix_etc'),
          ec2.InitCommand.shellCommand('cd /tmp/prefix_etc'),
          ec2.InitCommand.shellCommand(`unzip ${local_etc_path}`),
          ec2.InitCommand.shellCommand(
            'sudo cp /tmp/prefix_etc/setup-cert.service /etc/systemd/system/',
          ),

          ec2.InitCommand.shellCommand(
            'sudo cp /tmp/prefix_etc/sysctl.conf /etc/sysctl.d/evonytkr.conf',
          ),
          ec2.InitCommand.shellCommand('sudo service procps force-reload'),

          ec2.InitCommand.shellCommand(
            'sudo cp /tmp/prefix_etc/evonytkr.service /etc/systemd/system/',
          ),

          ec2.InitCommand.shellCommand(
            'cp /tmp/prefix_etc/logrotate-evonytkr.conf /etc/logrotate.d/evonytkr',
          ),
          ec2.InitCommand.shellCommand('chmod 644 /etc/logrotate.d/evonytkr'),

          ec2.InitCommand.shellCommand('sudo systemctl daemon-reload'),

          ec2.InitCommand.shellCommand(
            'sudo systemctl enable setup-cert.service',
          ),
          ec2.InitCommand.shellCommand('systemctl start setup-cert.service'),
          ec2.InitCommand.shellCommand('sudo /opt/prefix/bin/setup-nginx.sh'),
          ec2.InitCommand.shellCommand(
            'sudo systemctl enable evonytkr.service',
          ),
          ec2.InitCommand.shellCommand('systemctl start evonytkr'),
          ec2.InitCommand.shellCommand('systemctl reload nginx'),
        ]),
      },
    });

    this.initOptions = {
      configSets: ['default'],
      timeout: cdk.Duration.minutes(30),
      ignoreFailures: true, // Don't rollback on failure during debugging
    };
  }

  getRandomInteger(min: number, max: number): number {
    min = Math.ceil(min);
    max = Math.floor(max);
    return Math.floor(Math.random() * (max - min + 1)) + min;
  }
}
