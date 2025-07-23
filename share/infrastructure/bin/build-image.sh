#!/bin/bash -x

AWS_REGION='us-east-2';
AWS_ACCOUNT_ID='699040795025';
AWS_PROFILE='personal';
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
