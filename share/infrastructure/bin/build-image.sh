#!/bin/bash -x

AWS_REGION='us-east-2';
AWS_ACCOUNT_ID='699040795025';
AWS_PROFILE='personal';

# pnpm requires that the lock file and the package.json be in sync, ensure that by running pnpm i
pnpm i

# # Build your Mojolicious app image
podman build --platform linux/arm64 -t evonytkrtips:latest ../../

# Create ECR repository (if it doesn't exist)
if aws --profile personal --region 'us-east-2' ecr describe-repositories | jq '.repositories.[].repositoryName' | grep -q evonytkrtips ; then
  echo "repository already exists, not attempting creation."
else
  aws --profile ${AWS_PROFILE} --region ${AWS_REGION} ecr create-repository --repository-name evonytkrtips
fi

# Get ECR login credentials and login
aws --profile ${AWS_PROFILE} --region ${AWS_REGION} ecr get-login-password | podman login --username AWS --password-stdin ${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com || exit 1;

# Tag and push the image
podman tag evonytkrtips:latest ${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/evonytkrtips:latest
podman push ${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com/evonytkrtips:latest

DEV=false
PROD=false

OPTSTRING=":dp"

while getopts "${OPTSTRING}" opt; do
  case ${opt} in
    d)
      DEV=true
      ;;
    p)
      PROD=true
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done

# Get all cluster ARNs
CLUSTERS=$(aws --profile personal --region us-east-2 ecs list-clusters | jq -r '.clusterArns[]')

# Loop through each cluster
for CLUSTER in $CLUSTERS; do
  # For dev environments
  if $DEV && echo "$CLUSTER" | grep -q -- '-dev-'; then
    echo "Processing dev cluster: $CLUSTER"

    # Get the first service in the cluster
    SERVICE=$(aws --profile personal --region us-east-2 ecs list-services --cluster "$CLUSTER" | jq -r '.serviceArns[0]')

    if [ -n "$SERVICE" ]; then
      echo "Forcing new deployment for service: $SERVICE"
      aws --profile personal --region us-east-2 ecs update-service \
        --no-cli-pager \
        --cluster "$CLUSTER" \
        --service "$SERVICE" \
        --force-new-deployment
    else
      echo "No services found in cluster: $CLUSTER"
    fi
  fi

  # For prod environments
  if $PROD && echo "$CLUSTER" | grep -q -- '-prod-'; then
    echo "Processing prod cluster: $CLUSTER"

    # Get the first service in the cluster
    SERVICE=$(aws --profile personal --region us-east-2 ecs list-services --cluster "$CLUSTER" | jq -r '.serviceArns[0]')

    if [ -n "$SERVICE" ]; then
      echo "Forcing new deployment for service: $SERVICE"
      aws --profile personal --region us-east-2 ecs update-service \
        --no-cli-pager \
        --cluster "$CLUSTER" \
        --service "$SERVICE" \
        --force-new-deployment
    else
      echo "No services found in cluster: $CLUSTER"
    fi
  fi
done
