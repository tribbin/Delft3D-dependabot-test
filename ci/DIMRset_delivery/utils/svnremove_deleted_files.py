'''
Description: Path helper
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2015
'''

import os, re, sys, glob, argparse, getpass, subprocess
from pathlib import Path

#
# Get commandline arguments
parser = argparse.ArgumentParser(description='SVN remove deleted files.')

parser.add_argument("--source",
                    default="",
                    required=True,
                    help="Path to source directory",
                    dest="sourcedir")
parser.add_argument("--target",
                    default="",
                    required=True,
                    help="Path to source directory",
                    dest="targetdir")


# Username/password not needed for svn remove?
#   parser.add_argument("--username",
#                       help="Subversion username.",
#                       default=None,
#                       #required=True,
#                       dest="username")
#   parser.add_argument("--password",
#                       help="Subversion password.",
#                       default=None,
#                       #required=True,
#                       dest="password")
#   parser.add_argument('-i', '--interactive',
#                       help="Must be True to enable username/password via keyboard.",
#                       default=None,
#                       dest="interactive")

args = parser.parse_args()

#   print("u:" + str(args.__dict__["username"]))
#   print("p:" + str(args.__dict__["password"]))
#   print("i:" + str(args.__dict__["interactive"]))
#   
#   if args.__dict__["username"] != None:
#       username = args.__dict__["username"]
#   else:
#       if args.__dict__["interactive"] != None and str(args.__dict__["interactive"]) ==  "True":
#           username = input('Username for TeamCity access:')
#       else:
#           print('No username on commandline. add "-i True" to enable interactive input')
#           exit()
#   if args.__dict__["password"] != None:
#       password = args.__dict__["password"]
#   else:
#       if args.__dict__["interactive"] != None and str(args.__dict__["interactive"]) ==  "True":
#           password = getpass.getpass()
#       else:
#           print('No password on commandline. add "-i True" to enable interactive input')
#           exit()

# rootdir is the location of this script
rootdir = Path.cwd()

olddir = rootdir / Path(args.__dict__["targetdir"]).resolve()
newdir = rootdir / Path(args.__dict__["sourcedir"]).resolve()

if not olddir.is_dir():
   print("ERROR: target directory does not exist: " + olddir)
   exit(1)
if not newdir.is_dir():
   print("ERROR: source directory does not exist: " + newdir)
   exit(1)

for p in olddir.glob("**/*"):
    if not p.is_file(): continue
    lpfile = p.resolve()

    rel_p = lpfile.relative_to(olddir)
    lpfilenew = newdir / rel_p

    if not lpfilenew.is_file():
        cmd = "svn remove {}".format(lpfile)
        print(cmd)

        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, shell=True)
        (output, err) = process.communicate()
        print("  output:" + str(output))
        print("  err   :" + str(err))

print ("Finished")
