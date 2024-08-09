#!/bin/bash

IMAGE="containers.deltares.nl/delft3d/legacy/delft3dfm:latest"

# Pull the Docker image
docker pull $IMAGE

# List all subfolders in the current directory and run the script if it exists
for dir in */; do
    echo "Checking folder: $dir"
    if [ -f "$dir/run_docker.sh" ]; then
		cd $dir
        echo "Running run_docker.sh in $(pwd)"
        docker run -v ".:/data" $IMAGE
		cd ..
    else
        echo "No run script in $(pwd)"
    fi
done
