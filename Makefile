EMACS = emacs -Q -batch -L .
ELS = php-mode.el php-mode-test.el
ELCS = $(ELS:.el=.elc)

%.elc: %.el
	$(EMACS) -f batch-byte-compile $<

all: $(ELCS)

clean:
	rm -f $(ELCS)

# Runs all unit tests from php-mode-test.el and shows the results. The
# script will exit with the status code zero if all tests pass. If any
# test fails the script exits with a non-zero status and shows
# diagnostics on standard output.
#
# You can use this script with git-bisect. See the documentation at
#
#     http://git-scm.com/book/en/Git-Tools-Debugging-with-Git
#
# for an example of using a script like this with the 'git bisect run'
# command.
test: $(ELCS)
	$(EMACS) -l php-mode-test.el -f ert-run-tests-batch-and-exit

.PHONY: all clean test
