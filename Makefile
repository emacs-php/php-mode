EMACS ?= emacs
CASK ?= cask
ELS  = lisp/php.el
ELS += lisp/php-align.el
ELS += lisp/php-complete.el
ELS += lisp/php-defs.el
ELS += lisp/php-face.el
ELS += lisp/php-flymake.el
ELS += lisp/php-local-manual.el
ELS += lisp/php-mode-debug.el
ELS += lisp/php-mode.el
ELS += lisp/php-project.el
AUTOLOADS = php-mode-autoloads.el
ELCS = $(ELS:.el=.elc)

%.elc: %.el
	$(EMACS) --batch -L lisp/ -f batch-byte-compile $<

all: autoloads $(ELCS) authors

authors: AUTHORS.md

.PHONY: AUTHORS.md
AUTHORS.md: etc/git/AUTHORS.md.in .mailmap
	@printf "Generating AUTHORS.md file..."
	@test -d .git \
		&& (cat $< > $@ \
			&& git log --pretty=format:'- %aN' | \
			cat etc/git/former-contributors - | LANG=C sort -u >> $@ \
			&& cat etc/git/AUTHORS2.md.in >> $@ \
			&& printf "FINISHED\n" ; ) \
		|| printf "FAILED (non-fatal)\n"

autoloads: $(AUTOLOADS)

$(AUTOLOADS): $(ELS)
	$(EMACS) --batch -L lisp/ --eval \
	"(let ((user-emacs-directory default-directory)) \
	   (require 'package) \
	   (package-generate-autoloads \"php-mode\" (expand-file-name \"lisp\")))"

.cask: Cask
	$(CASK) install

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
	touch tests/project/1/.git
	$(EMACS) --batch -l lisp/php-mode-autoloads.el --eval \
	"(let ((default-directory (expand-file-name \".cask\" default-directory))) \
	   (normal-top-level-add-subdirs-to-load-path))" \
	    -l tests/php-mode-test.el -f ert-run-tests-batch-and-exit

.PHONY: all authors autoloads clean test
