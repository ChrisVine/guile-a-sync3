#!/bin/sh

LD_LIBRARY_PATH=../lib:$LD_LIBRARY_PATH
GUILE_LOAD_PATH=..:$GUILE_LOAD_PATH
export LD_LIBRARY_PATH GUILE_LOAD_PATH
# FIXME22 - guile22 is a temporary cludge
guile22 $1
