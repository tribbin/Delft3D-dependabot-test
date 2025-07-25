#!/bin/bash

# Initialize variables
image="containers.deltares.nl/delft3d/delft3dfm:daily"  # Default value

# Parse command-line arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --image) image="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

# Use the Docker image reference (Apptainer will cache automatically on first use)
apptainer_image="docker://$image"
echo "Using Docker image to run the apptainer: $apptainer_image (will be cached automatically)"

# Check if apptainer is available in PATH
if ! command -v apptainer &> /dev/null; then
    echo "Error: 'apptainer' command not found. Please install Apptainer and ensure it is in your PATH."
    exit 1
fi

# Loop through each subdirectory
for dir in */ ; do
    # Check if run_apptainer.sh exists in the subdirectory
    if [ -f "$dir/run_apptainer.sh" ]; then
        echo "Found run_apptainer.sh in $dir. Executing..."
        ${dir}run_apptainer.sh --image "$apptainer_image"
    else
        echo "No run_apptainer.sh found in $dir."
    fi
done