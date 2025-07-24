#!/bin/sh

# Set permissions on the ephemeral mount point
mkdir -p /home/mojo/var # for local testing
chown -R mojo:mojo /home/mojo/var
chmod -R 755 /home/mojo/var

# Execute the main application as the unprivileged user
pwd
exec sudo -D /opt/Game-EvonyTKR  -u mojo -- "$@"
