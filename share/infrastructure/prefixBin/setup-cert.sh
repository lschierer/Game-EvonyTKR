#!/bin/bash
set -e

H=$(hostname -s)
MAX_ATTEMPTS=20
ATTEMPT=1

# Check if real cert already exists (not the snake-oil cert)
if [ -L /etc/nginx/ssl/evonytkrtips.net.crt ] && [ -e /etc/nginx/ssl/evonytkrtips.net.crt ]; then
  SUBJECT=$(openssl x509 -in /etc/nginx/ssl/evonytkrtips.net.crt -noout -subject 2>/dev/null || echo "")
  if [[ "$SUBJECT" != *"CN=localhost"* ]]; then
    echo "Certificate already installed and valid"
    exit 0
  fi
  echo "Found snake-oil cert, will replace with real cert"
fi

# Wait for DNS with retries
while [ $ATTEMPT -le $MAX_ATTEMPTS ]; do
  echo "Attempt $ATTEMPT/$MAX_ATTEMPTS: Checking DNS for ${H}.evonytkrtips.net"
  
  DNS_RESULT=$(dig +short @8.8.8.8 ${H}.evonytkrtips.net A)
  echo "DNS result: '$DNS_RESULT'"
  
  if [ -n "$DNS_RESULT" ]; then
    echo "DNS is ready!"
    break
  fi
  
  if [ $ATTEMPT -eq $MAX_ATTEMPTS ]; then
    echo "DNS not ready after $MAX_ATTEMPTS attempts, giving up"
    exit 1
  fi
  
  SLEEP_TIME=$((30 + ATTEMPT * 10))
  echo "DNS not ready yet, sleeping ${SLEEP_TIME}s..."
  sleep $SLEEP_TIME
  ATTEMPT=$((ATTEMPT + 1))
done

# Run certbot with retries
ATTEMPT=1
while [ $ATTEMPT -le 5 ]; do
  echo "Attempt $ATTEMPT/5: Running certbot"
  
  DOMAIN2="REPLACE_DOMAIN"
  if certbot certonly --nginx -m 'staff@schierer.org' --agree-tos -d "${H}.evonytkrtips.net" -d "${DOMAIN2}" -d "www.${DOMAIN2}" --non-interactive; then
    # Only replace certs if certbot succeeded
    rm -f /etc/nginx/ssl/evonytkrtips.net.key /etc/nginx/ssl/evonytkrtips.net.crt
    ln -s /etc/letsencrypt/live/${H}.evonytkrtips.net/privkey.pem /etc/nginx/ssl/evonytkrtips.net.key
    ln -s /etc/letsencrypt/live/${H}.evonytkrtips.net/fullchain.pem /etc/nginx/ssl/evonytkrtips.net.crt
    systemctl reload nginx
    echo "Certificate installed successfully"
    exit 0
  fi
  
  if [ $ATTEMPT -eq 5 ]; then
    echo "Certbot failed after 5 attempts, giving up"
    exit 1
  fi
  
  echo "Certbot failed, retrying in 30s..."
  sleep 30
  ATTEMPT=$((ATTEMPT + 1))
done
