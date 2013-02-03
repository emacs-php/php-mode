#!/bin/sh
#
# This shell script runs all of unit tests from the php-mode-test.el
# file and shows the results.  The script will exit with the status
# code zero if all tests pass.  If any test fails the script exits
# with a non-zero status and shows diagnostics on standard output.
#
# You can use this script with git-bisect.  See the documentation at
#
#     http://git-scm.com/book/en/Git-Tools-Debugging-with-Git
#
# for an example of using a script like this with the 'git bisect run'
# command.

emacs -Q -batch -l cl -l ert \
    -l php-mode.el \
    -l php-mode-test.el \
    -f ert-run-tests-batch-and-exit
