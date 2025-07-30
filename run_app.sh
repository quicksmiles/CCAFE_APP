#!/usr/bin/env bash

# this script first builds a Docker image for the project, then runs it,
# attaching to the logs for the container. pressing ctrl-c in the terminal will
# stop the container and remove it.

# the first build will be slow (~13 minutes on my m1 macbook pro), but
# subsequent builds should be fast thanks to docker's layer caching mechanism.

# to access the site once the container is running, go to:
# http://localhost:3850/ccafe/

IMAGE_TAG=latest
IMAGE=us-central1-docker.pkg.dev/cuhealthai-sandbox/hendrickslab/ccafe_app:${IMAGE_TAG}

docker build --platform linux/amd64 -f ./docker/Dockerfile -t ${IMAGE} . &&
docker run  --platform linux/amd64 --name ccafe_app --rm -it \
    -p 3850:3838 \
    -v $(pwd):/srv/shiny-server/app/ \
    -e APP_ROOT=/srv/shiny-server/app \
    ${IMAGE}
