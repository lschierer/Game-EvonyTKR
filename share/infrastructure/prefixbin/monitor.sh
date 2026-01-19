#!/bin/bash
set -e

while [ 1 ]; do
  clear;
  uptime;
  free -m;
  sudo -u mojo /opt/mojo/bin/minion-job.sh -s;
  echo 'failed jobs:'
  sudo -u mojo /opt/mojo/bin/minion-job.sh -S failed;
  echo 'active jobs:'
  sudo -u mojo /opt/mojo/bin/minion-job.sh -S active;
  echo;
  sudo -u mojo tail -n 20 /opt/mojo/var/log/Perl/dist/Game-EvonyTKR/system.log;
  sleep 5;
done
