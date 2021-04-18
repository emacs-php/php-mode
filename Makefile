EMACS ?= emacs
CASK ?= cask
ELS = lisp/php.el lisp/php-align.el lisp/php-face.el lisp/php-project.el lisp/php-local-manual.el lisp/php-mode.el lisp/php-ui.el lisp/php-ui-phpactor.el lisp/php-mode-debug.el
AUTOLOADS = php-mode-autoloads.el
ELCS = $(ELS:.el=.elc)

%.elc: %.el
	$(EMACS) -Q -batch -L lisp/ --eval \
	"(let ((default-directory (expand-file-name \".cask\" default-directory))) \
	   (require 'package) \
	   (normal-top-level-add-subdirs-to-load-path))" \
	-f package-initialize -f batch-byte-compile $<

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

$(AUTOLOADS): lisp/php.el lisp/php-align.el lisp/php-face.el lisp/php-project.el lisp/php-local-manual.el lisp/php-mode-debug.el lisp/php-mode.el
	$(EMACS) -Q -batch -L lisp/ --eval \
	"(progn \
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
	$(EMACS) -Q -batch -L lisp/ --eval \
	"(let ((default-directory (expand-file-name \".cask\" default-directory))) \
	   (require 'package) \
	   (normal-top-level-add-subdirs-to-load-path))" \
	    -f package-initialize \
	    -l tests/php-mode-test.el -f ert-run-tests-batch-and-exit

.PHONY: all authors autoloads clean test
