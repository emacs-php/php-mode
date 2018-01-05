EMACS ?= emacs
ELS = php-mode.el php-mode-test.el
ELCS = $(ELS:.el=.elc)

%.elc: %.el
	$(EMACS) -Q -batch -L . -f batch-byte-compile $<

all: autoloads $(ELCS)

autoloads: php-mode-autoloads.el

php-mode-autoloads.el:
	$(EMACS) -Q -batch -L . --eval \
	"(progn \
           (require 'package) \
           (package-generate-autoloads \"php-mode\" default-directory))"

clean:
	rm -f $(ELCS) php-mode-autoloads.el

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
test: clean all
	$(EMACS) -Q -batch -L . -l php-mode-test.el -f ert-run-tests-batch-and-exit

.PHONY: all autoloads clean test
