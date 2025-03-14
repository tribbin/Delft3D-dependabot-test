import argparse
import glob
import os
import shutil
import sys

# Set up argument parser
parser = argparse.ArgumentParser(description="Copy examples to a specified location.")
parser.add_argument(
    "dest_dir", help="The destination directory where examples will be copied."
)
args = parser.parse_args()

DEST_DIR = args.dest_dir
EXAMPLES_DIR = "examples\\dflowfm"
LATEST_DIR = "src\\scripts_lgpl\\singularity"

print(f"Copy examples to location {DEST_DIR}")

# Check if DEST_DIR is defined
if not DEST_DIR:
    print("DEST_DIR is not defined.")
    sys.exit(1)

# Check if the destination directory exists
if not os.path.exists(DEST_DIR):
    print("Directory does not exist, creating...")
    try:
        os.makedirs(DEST_DIR)
    except OSError:
        print(f"Unable to create directory - {DEST_DIR}")
        sys.exit(1)
else:
    print("Directory exists, deleting contents...")
    try:
        for root, dirs, files in os.walk(DEST_DIR):
            for name in files:
                path = os.path.join(root, name)
                print(f"Removing: {path}")
                os.remove(path)
            for name in dirs:
                path = os.path.join(root, name)
                print(f"Removing: {path}")
                shutil.rmtree(path)
    except OSError as e:
        print(f"Failed to delete contents of directory - {DEST_DIR}: {e}")
        sys.exit(1)

print("Exclude over all run scripts")
run_all_examples_pattern = os.path.join("examples", "dflowfm", "run-all-examples-*")
for file in glob.glob(run_all_examples_pattern):
    try:
        print(f"Removing file: {file}")
        os.remove(file)
    except OSError:
        pass

print("Copy files from the examples directory to the destination")
latest_scripts = ["run_native_h7.sh", "submit_singularity_h7.sh"]
try:
    for root, dirs, files in os.walk(EXAMPLES_DIR):
        for name in files:
            src_file = os.path.join(root, name)
            dest_file = os.path.join(DEST_DIR, os.path.relpath(src_file, EXAMPLES_DIR))
            dest_dir = os.path.dirname(dest_file)
            print(f"Copying file: {src_file} to {dest_file}")
            shutil.copy2(src_file, dest_file)
        for name in dirs:
            src_dir = os.path.join(root, name)
            dest_dir = os.path.join(DEST_DIR, os.path.relpath(src_dir, EXAMPLES_DIR))
            if not os.path.exists(dest_dir):
                print(f"Creating directory: {dest_dir}")
                os.makedirs(dest_dir)
        for root, dirs, files in os.walk(DEST_DIR):
            if root == DEST_DIR:
                for name in dirs:
                    dest_dir = os.path.join(root, name)
                    for file in glob.iglob(os.path.join(LATEST_DIR, "*.sh")):
                        if os.path.basename(file) in latest_scripts:
                            print(f"Copying file: {file} to {dest_dir}")
                            shutil.copy2(file, dest_dir)
except shutil.Error as e:
    print(f"Copy failed: {e}")
    sys.exit(1)

print("Copy completed.")
