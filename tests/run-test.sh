#!/bin/sh

LD_LIBRARY_PATH=../lib:$LD_LIBRARY_PATH
GUILE_LOAD_PATH=..:$GUILE_LOAD_PATH
export LD_LIBRARY_PATH GUILE_LOAD_PATH
guile $1
