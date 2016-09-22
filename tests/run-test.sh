#!/bin/sh

# There is a bug in how guile-2.1.4 deals with compiled code for files
# in a path given by the GUILE_LOAD_PATH environmental variable, which
# means that it is not possible to run the tests with 'make test'
# using the locally compiled code in the source directory.  Instead it
# is necessary to install guile-a-sync2 first and run 'make test'
# after installation.  So for the moment we comment out the local
# source directory for the tests.

#LD_LIBRARY_PATH=../lib:$LD_LIBRARY_PATH
#GUILE_LOAD_PATH=..:$GUILE_LOAD_PATH
#export LD_LIBRARY_PATH GUILE_LOAD_PATH

guile $1
