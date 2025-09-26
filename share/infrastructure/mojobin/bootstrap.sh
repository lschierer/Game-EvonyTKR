#! /bin/bash -x

touch /opt/mojo/.bash_profile

curl https://mise.run | sh

echo "eval \"\$(/opt/mojo/.local/bin/mise activate bash)\"" >> ~/.bashrc

eval "$(/opt/mojo/.local/bin/mise activate bash)"

mise reshim

#diagnostic, not actually part of the install
mise doctor

echo 'eval \"\$(/opt/mojo/.local/bin/mise activate bash)\"' >> ~/.bash_profile
echo 'export PATH="/opt/mojo/.local/bin/:$HOME/bin:$PATH"' >> /opt/mojo/.bash_profile

export PATH="/opt/mojo/.local/bin/:$HOME/bin:$PATH"

git clone -b streaming https://github.com/lschierer/Game-EvonyTKR.git /opt/mojo/app

cd /opt/mojo/app

mise trust
mise install

cpanm -n utf8::all Module::Build

cpanm -n IO::Socket::SSL

just build
