#!/bin/bash

scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`

$scriptdir/run_dflowfm.sh $*
