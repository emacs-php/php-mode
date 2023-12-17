EMACS ?= emacs
CASK ?= cask
EASK ?= eask

compile:
	$(EASK) compile

all: autoloads $(ELCS) authors

authors: AUTHORS.md

.PHONY: AUTHORS.md
AUTHORS.md: etc/git/AUTHORS.md.in .mailmap
	@printf "Generating AUTHORS.md file..."
	@test -d .git \
		&& (cat $< > $@ \
			&& git log --pretty=format:'- %aN' \
				| cat etc/git/former-contributors - \
				| grep -v dependabot \
				| LANG=C sort -u >> $@ \
			&& cat etc/git/AUTHORS2.md.in >> $@ \
			&& printf "FINISHED\n" ; ) \
		|| printf "FAILED (non-fatal)\n"

autoloads:
	$(EASK) generate autoloads

.eask: Eask
	$(EASK) install

clean:
	$(EASK) clean all

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
	$(EASK) test ert ./tests/php-mode-test.el

.PHONY: all authors autoloads clean test
