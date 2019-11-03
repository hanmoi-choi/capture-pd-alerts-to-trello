export APP_NAME="capture-pd-alerts-to-trello"

export ECR_REGISTRY_ACCOUNT_ID="639347700193"
export ECR_REGISTRY_BASE_NAME=".dkr.ecr.ap-southeast-2.amazonaws.com"
export ECR_REGISTRY="${ECR_REGISTRY_ACCOUNT_ID}${ECR_REGISTRY_BASE_NAME}/property-insights/capture-pd-alerts-to-trello"
export CURRENT_ECR_DOCKER_TAG=${ECR_REGISTRY}:latest

export DEV_CONTAINER_IMAGE_TAG=dev-latest
export PROD_CONTAINER_IMAGE_TAG=latest
