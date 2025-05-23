import argparse
import os
import string
import sys
from datetime import datetime

"""
This script list all what strings, starting with @(#)Deltares, in all subdirectories of a given root directory
The root directory is specified by the argument --srcdir .....
"""


def write_log(*args, **kwargs):
    log_file.write(" ".join(map(str, args)) + "\n")


def extract_printable_strings(filename, min_length=4):
    with open(filename, "r", errors="ignore") as f:
        printable_set = set(string.printable)
        result = ""
        for c in f.read():
            if c in printable_set:
                result += c
                continue
            if len(result) >= min_length:
                yield result
            result = ""
        # Ensure the last accumulated string is yielded if it meets the minimum length requirement
        if len(result) >= min_length:
            yield result


def list_what_strings(file_path: str) -> None:
    print(f"\t\t{file_path}")
    string_list = list(extract_printable_strings(file_path))
    what_string = []
    for s in string_list:
        if s.find("@(#)Deltares") != -1:
            what_string.append(s[s.find("@(#)Deltares") :])
        if s.find("HeadURL") != -1:
            what_string.append(s[s.find("HeadURL") :])
    if len(what_string) != 0:
        write_log("\t%s" % file_path)
        for s in what_string:
            if len(s) >= 4 and s[0:4] == "@(#)":
                write_log("\t\t%s" % s[4:])
            if len(s) >= 7 and s[0:7] == "HeadURL":
                write_log("\t\t%s" % s[9:])


def walk_and_list_what_strings(root_folder):
    for current_dir, subdirs, files in os.walk(root_folder):
        if current_dir != root_folder:
            print(f"\t{current_dir}")
            for file_name in files:
                file_path = os.path.join(current_dir, file_name)
                list_what_strings(file_path)


def get_command_line_args():
    parser = argparse.ArgumentParser(
        description="Batch process to list all what-strings"
    )
    parser.add_argument(
        "-s",
        "--srcdir",
        help="Root directory from the what-strings are listed",
        dest="src_dir",
    )
    parser.add_argument("-o", "--output", help="Output filename.", dest="out_put")
    args = parser.parse_args()
    return args


if __name__ == "__main__":
    start_time = datetime.now()

    args = get_command_line_args()

    src_dir = "."
    out_put = "dimr_version.txt"
    if args.src_dir:
        src_dir = args.src_dir
    start_dir = os.getcwd()
    src_dir = os.path.normpath(os.path.join(start_dir, src_dir))
    if not os.path.exists(src_dir):
        print("Given directory does not exists: %s" % src_dir)

    if args.out_put:
        out_put = args.out_put
    if os.path.exists(out_put):
        os.remove(out_put)
    log_file = open(out_put, "a")

    print("Start: %s\n" % start_time)
    write_log("Start: %s\n" % start_time)
    print("%s\n" % sys.version)

    print("Listing is written to: %s" % out_put)
    os.chdir(src_dir)

    print("Root Directory: %s" % src_dir)
    write_log("Root Directory: %s" % src_dir)
    walk_and_list_what_strings(src_dir)
    print("Processing done")
    write_log("Processing done")

    os.chdir(start_dir)

    write_log("\nStart: %s" % start_time)
    write_log("End  : %s" % datetime.now())
    write_log("Klaar")
    print("\nStart: %s" % start_time)
    print("End  : %s" % datetime.now())
    print("Klaar")
