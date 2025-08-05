#!/bin/bash -x

AWS_REGION='us-east-2'
AWS_ACCOUNT_ID='699040795025'
AWS_PROFILE='personal'
REPO_NAME='evonytkrtips'
SOURCE_TAG='latest'
TARGET_TAG='stable'

# Fetch the manifest of the source image
MANIFEST=$(aws --profile "${AWS_PROFILE}" --region "${AWS_REGION}" \
  ecr batch-get-image \
  --repository-name "${REPO_NAME}" \
  --image-ids imageTag="${SOURCE_TAG}" \
  --query 'images[0].imageManifest' \
  --output text)

# Put the manifest under the new tag
aws --profile "${AWS_PROFILE}" --region "${AWS_REGION}" \
  ecr put-image \
  --repository-name "${REPO_NAME}" \
  --image-tag "${TARGET_TAG}" \
  --image-manifest "${MANIFEST}"

# Discover all ECS clusters
CLUSTERS=$(aws --profile "${AWS_PROFILE}" --region "${AWS_REGION}" ecs list-clusters | jq -r '.clusterArns[]')

# Loop through each cluster looking for prod
for CLUSTER in $CLUSTERS; do
  if echo "$CLUSTER" | grep -q -- '-prod-'; then
    echo "Triggering deploy in prod cluster: $CLUSTER"

    SERVICE=$(aws --profile "${AWS_PROFILE}" --region "${AWS_REGION}" ecs list-services --cluster "$CLUSTER" | jq -r '.serviceArns[0]')

    if [ -n "$SERVICE" ]; then
      echo "Forcing new deployment for service: $SERVICE"
      aws --profile "${AWS_PROFILE}" --region "${AWS_REGION}" ecs update-service \
        --no-cli-pager \
        --cluster "$CLUSTER" \
        --service "$SERVICE" \
        --force-new-deployment
    else
      echo "No services found in cluster: $CLUSTER"
    fi
  fi
done
