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

# Loop through each subdirectory
for dir in */ ; do
    # Check if run_apptainer.sh exists in the subdirectory
    if [ -f "$dir/run_apptainer.sh" ]; then
        echo "Found run_apptainer.sh in $dir. Executing..."
        # Run the script
        ${dir}/run_apptainer.sh --image "$image"
    else
        echo "No run_apptainer.sh found in $dir."
    fi
done