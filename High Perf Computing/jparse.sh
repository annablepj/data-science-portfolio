#!/bin/sh

if [ "$#" -eq 0 ]; then
  echo "Usage:  jparse <input file>"
  exit 1;
fi
#
# use tr to remove all non-ascii characters from file
# 
jq -r -f ./cmds.jq $1 | tr -cd '\11\12\15\40-\176' 
