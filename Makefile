EMACS ?= emacs
ELS = php-project.el php-mode.el php-mode-test.el
AUTOLOADS = php-project-autoloads.el php-mode-autoloads.el
ELCS = $(ELS:.el=.elc)

%.elc: %.el
	$(EMACS) -Q -batch -L . -f batch-byte-compile $<

all: autoloads $(ELCS)

autoloads: $(AUTOLOADS)

$(AUTOLOADS): php-project.el php-mode.el
	$(EMACS) -Q -batch -L . --eval \
	"(progn \
	   (require 'package) \
	   (package-generate-autoloads \"php-mode\" default-directory))"

clean:
	rm -f $(ELCS) $(AUTOLOADS)

# Perform any operations that will be useful for developers
# who contribute to PHP Mode.
dev:
	cp etc/git/prepare-commit-msg .git/hooks/prepare-commit-msg
	chmod u+x .git/hooks/prepare-commit-msg
	
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
