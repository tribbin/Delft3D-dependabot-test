import argparse
import glob
import shutil
import sys
from pathlib import Path

EXAMPLES_DIR = Path("examples/dflowfm")
# this dir is the source of thurth the scripts in the exampels dir in git should be removed
LATEST_DIR = Path("src/scripts_lgpl/singularity")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Copy examples to a specified location."
    )
    parser.add_argument(
        "dest_dir", help="The destination directory where examples will be copied."
    )
    args = parser.parse_args()

    dest_dir = Path(args.dest_dir)

    print(f"Copy examples to location {dest_dir}")

    if not dest_dir:
        print("DEST_DIR is not defined.")
        sys.exit(1)

    if not dest_dir.exists():
        print("Directory does not exist, creating...")
        try:
            dest_dir.mkdir(parents=True, exist_ok=True)
        except OSError:
            print(f"Unable to create directory - {dest_dir}")
            sys.exit(1)
    else:
        print("Directory exists, deleting contents...")
        try:
            for item in dest_dir.iterdir():
                if item.is_file():
                    print(f"Removing: {item}")
                    item.unlink()
                elif item.is_dir():
                    print(f"Removing: {item}")
                    shutil.rmtree(item)
        except OSError as e:
            print(f"Failed to delete contents of directory - {dest_dir}: {e}")
            sys.exit(1)

    print("Exclude over all run scripts")
    run_all_examples_pattern = EXAMPLES_DIR / "run-all-examples-*"
    for file in glob.glob(str(run_all_examples_pattern)):
        try:
            print(f"Removing file: {file}")
            Path(file).unlink()
        except OSError:
            pass

    print("Copy files from the examples directory to the destination")
    latest_scripts = ["run_native_h7.sh", "submit_singularity_h7.sh"]
    try:
        for src_file in EXAMPLES_DIR.rglob("*"):
            dest_file = dest_dir / src_file.relative_to(EXAMPLES_DIR)
            if src_file.is_file():
                dest_file.parent.mkdir(parents=True, exist_ok=True)
                print(f"Copying file: {src_file} to {dest_file}")
                shutil.copy2(src_file, dest_file)
            elif src_file.is_dir():
                dest_file.mkdir(parents=True, exist_ok=True)
                print(f"Creating directory: {dest_file}")

        for dest_subdir in dest_dir.rglob("*"):
            if dest_subdir.is_dir():
                for file in LATEST_DIR.glob("*.sh"):
                    if file.name in latest_scripts:
                        print(f"Copying file: {file} to {dest_subdir}")
                        shutil.copy2(file, dest_subdir)
    except shutil.Error as e:
        print(f"Copy failed: {e}")
        sys.exit(1)

    print("Copy completed.")
