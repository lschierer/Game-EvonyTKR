#!/bin/bash
set -e
H=$(hostname -s)

# Check if real cert already exists
if [ -L /etc/nginx/ssl/evonytkrtips.net.crt ] && [ -e /etc/nginx/ssl/evonytkrtips.net.crt ]; then
  echo "Certificate already installed and valid"
  exit 0
fi

# Check DNS is ready
if ! dig +short @8.8.8.8 ${H}.evonytkrtips.net A | grep -q .; then
  echo "DNS not ready yet for ${H}.evonytkrtips.net"
  exit 1
fi

# Run certbot
DOMAIN2=$(echo "REPLACE2.evonytkrtips.net")
if certbot certonly --nginx -m 'staff@schierer.org' --agree-tos -d "${H}.evonytkrtips.net" -d "${DOMAIN2}" -d "www.${DOMAIN2}" --non-interactive; then
  # Only replace certs if certbot succeeded
  rm -f /etc/nginx/ssl/evonytkrtips.net.key /etc/nginx/ssl/evonytkrtips.net.crt
  ln -s /etc/letsencrypt/live/${H}.evonytkrtips.net/privkey.pem /etc/nginx/ssl/evonytkrtips.net.key
  ln -s /etc/letsencrypt/live/${H}.evonytkrtips.net/fullchain.pem /etc/nginx/ssl/evonytkrtips.net.crt
  systemctl reload nginx
  echo "Certificate installed successfully"
else
  echo "Certbot failed, will retry"
  exit 1
fi
