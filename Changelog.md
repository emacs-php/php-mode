Changes for PHP Mode by Version
===============================

**Note:** This document does not list every change from every version,
because when I (Eric James Michael Ritz) took over the project I did
not have a record of all changes available.

1.18.3
------

* Fix warning over `imenu--make-index-alist` (#336)
* Fix highlighting of PHPDoc style comments
* Small improvements and corrects to the README
* Allow customizing `php-site-url`
* Refactored `php-search-web-documentation` and `php-browse-manual`
* Highlight nullable types in docblocks
* Fix a warning when compiling PHP Mode with GNU Emacs 25.1
* Fix the list of reserved keywords
* Change `font-lock-*-face`s to customizable `php-*-face`s
	
1.18.2
------

* Fix namespace indentation issue(#335)

1.18.1
------

* Fix hung up issue(#333)

1.18.0
------

* Improve PHPDoc highlighting
* Fix this, that highlighting
* Drop Emacs 23 support
* Improve Emacs 25 support
* Support PHP 7 syntax highlighting
* Fix closure indentation issue
* Improve php-xmlparser.el
* Support 'strict_types'
* Support 'yield from' keyword


1.13
----

* Update version to 1.13
* Merge branch 'ejmr/highlight-variables-in-strings'
* Merge commit '38e1940e950d47737fed30a5bd5d4e75e0faf103'
* Append file patterns to auto-mode-alist
* Guard propertize functions for Emacs that don't support it.
* Add more tests for highlighting variable interpolation
* Create a unit test for highlighting interpolated variables
* Avoid calling syntax-propertize-rules as may be unavailable.
* README: Explicitly mention the oldest supported Emacs
* Merge branch 'heredoc-support'
* README: Replace mention of ‘run-tests.sh’ with ‘make tests’
* Explain the expected behavior of the test for GitHub issue 124
* Added unit test for GitHub issue 124
* Replaced run-tests.sh with a Makefile
* Replaced `put-text-property' with macro `c-put-char-property'
* Added initial support for propertizing a heredoc as a string
* Merge branch 'fix-highlighting-final-keyword'
* Update php-mode-modified
* Add a simple script to test syntax highlighting for ‘final’
* Add a template script for creating test cases
* Fix ‘final’ not highlighted as a keyword in some methods
* Merge branch 'github/pr/127'
* Update php-mode-modified
* Removed unused defconst.
* Added PREG constants to the list of predefined constants.
* Fix byte compile error again; resurfaced in after a merge.
* Add Andrei Chițu to the list of contributors
* fix inclusion of `web-mode-extra-php-keywords` in `php-keywords`
* Update Changelog for version 1.12

1.12
----

* Automatically enable PHP Mode for Amaka scripts
* Also enable PHP Mode for Amaka scripts using the ‘*.amk’ extension
* Use mode-specific functions for defun movement
* Treat `const` as a keyword instead of a type-hint
* Add `enddeclare` to the list of keywords
* Treat `eval` as a built-in keyword
* Show links to the official site and wiki when customizing PHP Mode
* Update php-mode-modified for changes made today
* Add FILTER_SANITIZE_FULL_SPECIAL_CHARS to the list of constants
* Fix an error about php-extra-constants being void
* Use extra constants and keywords from Web Mode if available
* README: Replace ‘php-mode’ with ‘PHP Mode’ for consistency
* README: Update the ‘Status’ section
* README: Use the spelling ‘GitHub’ consistently
* README: Add installation instructions
* Address the compiler warning regarding `c-syntactic-context`
* GitHub-Issue: 102 (Unit Test)
* Fix indentation error involving magic constants
* Merge branch 'ejmr/issue-102'
* Stop marching indentation for try-catch blocks
* GitHub-Issue: 100 (Unit Test)
* Fix two errors with the regex for matching namespace imports
* GitHub-Issue: 115 (Unit Test)
* Use the PEAR indentation style by default for unit tests
* Align chained method calls inside of arrays
* Update php-mode-modified
* Merge branch 'ejmr/align-method-calls-in-arrays'
* Update php-mode-modified for byte-compiler fixes
* Merge branch 'github/pr/123'
* README: Document use of Web Mode constants and keywords
* Merge branch 'ejmr/web-mode-constants'
* Change the test for Issue 100 to require correct indentation
* Line-up multiple namespaces in a multi-line ‘use’ statement
* Merge branch 'ejmr/multiline-use-statements'
* Increase the version number to 1.12
* Fix the compiler warning about `add-log-current-defun-header-regexp`
* Add (require 'speedbar) as functions are used.
* Remove eval-when-compile.
* Add (require 'etags) as etag functions are used.
* Added newline at end of file restriction to Symfony2 style.
* Fix regex for namespaces in function calls


1.11
----

* Add STDOUT, STDIN, and STDERR to the php-constants list
* Add Symfony2 coding style.
* Add a full copy of the license
* Add proper statement-case-intro indent for symfony2
* Added "function" as a PHP keyword.
* Added function php-lineup-arglist
* Added gitignore with *.elc
* Associate file extensions when installing from an Elisp repository
* Be nice to PSR standards (there should be 4 spaces)
* Correct indentation for array arguments (tests/issues-14.php)
* Correct syntax highlighting for ‘parent’ and ‘self’
* Describe php-extra-constants in the README
* Describe php-template-compatibility in the README
* Do not expect failure for test issue-19
* Do not trigger search error in php-mode-test-issue-19
* Document Subword Mode in the README
* Document support for the Symfony2 style in the README
* Fix chained method alignment
* Fix indentation of statements after ‘foreach’ without braces
* Highlight ‘static’ as a constant when it appears in a class context
* Improve the docstring for php-create-regexp-for-method
* Improve the plain-text formatting of the README
* Introduce php-extra-constants
* Introduce php-template-compatility
* Introduce ‘C-c C-w’ to toggle Subword Mode
* Issue #73, correct behavior of `delete-indentation`.
* List all methods via Imenu regardless of their visibility
* Make `with-php-mode-test` aware of the Symfony2 coding style
* README: Change the absolute link to the Changelog to a relative link
* README: Correct the documentation for chained-method call alignment
* README: Document how chained method alignment may fail
* README: Reword the mention of Web Mode for clarity
* README: Use syntax highlighting for the method alignment example
* Re-define C-M-h to mark-defun instead of c-mark-function
* Remove the unnecessary &optional from the with-php-mode-test macro
* Set brace-list-entry to offset of four
* Tests: Remove executable permissions from the issue 27 test
* Treat ‘abstract’ as a keyword
* Use "magic" comments in PHP files to simplify indentation testing
* Use magic for tests of issues #14, #19, #27, #29, #42
* Workaround "bug" in `load-theme`


1.10
----

* Add `php-mode-coding-style` so users can customize their preferred
  coding style.
* Fix a bug that messes up the indentation of some closures.
* Update php-mode for Emacs 24 with regard to changes to the `cl`
  Elisp library.
* Submit php-mode to the [MELPA](http://melpa.milkbox.net/)
  repository.
* Fix indentation of `foreach` blocks that have only a single
  statement following them, without any braces.
* Add all valid flags for `htmlspecialchars()` to the list of
  constants.
* Improve indentation of chained method calls, although this still has
  an issue to iron out.
* Provide a test suite for developers working on php-mode to help
  improve quality control.
* Add all constants for errors.
* Add all constants for the cURL module.
* Fix a bug where some methods are not highlighted as functions.
* Remove `then` as a PHP keyword, since it is not a valid keyword.
* Fix annotation highlighting when using C-style `/*...*/` comments.
* Redefine the php-mode coding styles using cc-mode.
* Fix a bug that applies incorrect syntax highlighting to certain PHP
  keywords when they appear in block comments, e.g. `continue`.
* Fix a bug where php-mode incorrectly highlights variables and
  methods inside of comments.
* Treat acceptable directives to `declare()` as keywords.


1.9
---

* Add all new PHP 5.5 keywords.
* Add new test cases for indentation and highlighting.
* Add new PHP 5.5 `PASSWORD_*` constants.
* Do not treat function-like language constructs as functions,
  e.g. `array()`.
* Treat `callable` as a valid type hint.
* Fix an error regarding the requirement of `flymake`.
* Fix a syntax error in `php-unindent-closure`.
* Add support Drupal and WordPress coding styles.
* Document how to change the default coding style in the README.
* Fix an issue with running hooks after loading `php-mode`.
* Properly fontify function calls using one namespace qualifier.


1.7 and 1.8
-----------

* Line up cascaded method calls.
* Add test cases for indentation features.
* Fix syntax highlighting for multiline strings.
* Fix compiler warnings for:
  - `font-lock-syntactic-keywords`
  - `c-vsemi-status-unknown-p`
* Support highlighting in annotations in comments.
* Fix the ‘#’ sign as a comment delimeter.
* Improve indentation for anonymous functions.
* Remove unused libraries.
* Remove a corrupted character from a docstring.


1.6.6
-----

* Improve indentation of functions and arrays.
* Fix bug displaying strings in back-ticks.
* Add `php-function-call-face`.
* Add IMAP related constants.
* Fix fontification of function arguments to work across multiple
  lines, including type-hints.
* Fix bug when trying to browse online documentation.
* Allow sending code to PHP directly for execution.
* Add `die` to the list of language constructs.
* Use `font-lock-syntatic-keywords` to detect quoted strings.


1.6.5
-----

There was no version 1.6.5 release due to poor planning on my part
(EJMR).


1.6.4
-----

* Make it possible to use namespaces in more places.
* Fix Elisp warnings about `save-excursion` and `set-buffer`.
* Properly fontify the ‘instanceof’ operator.
* Add function to search local PHP documentation.
* Try searching local documentation before searching the manual
  on PHP.net online.
* Add PHP to `interpreter-mode-alist`.
* Improve indentation of nested arrays.
* Add `.phpt` to `auto-mode-alist`.


1.6.3
-----

* Fix bug with regular vs. static function highlighting.
* Add all `FILTER_*` and `INPUT_*` constants.
* Add `php-mode-warn-if-mumamo-off`.
* Fix fontification for namespace imports.
* Do not warn on constructs like `$foo->$bar`.
* Fontify expressions like `const Foo = 1`.
* Fix fontification for `return new Foo()`.


1.6.2
-----

* Add notes on Github for contributing to php-mode.
* Highlight ‘use <trait>’ in class definitions.
* Add Flymake support.


1.6.1
-----

* Support as magic and pre-defined constants.
* Add ‘default’ as a keyword.
* Add ‘clone’ as a keyword.
* Align cascading function calls.
* List methods in Imenu.
* Show variables assigned to anonymous functions in Imenu.
* Show interfaces in Imenu.
* Prevent methods from appearing under ‘Named Functions’ on Imenu.


1.6
---

* Fix most definitions in `php-font-lock-keywords-2`.
* Fix highlighting of namespace imports.
* Treat ‘as’ and ‘use’ as keywords.


1.5.1
-----

* Add `__NAMESPACE__` to the list of constants.
* Fontify ‘use <namespace> as <alias>’ constructs.
* Fix highlighting for namespaces, classes, and traits.
* Allow backslashes in namespaces for Imenu.


1.5.0-nxhtml-1.88
-----------------

* Do not indent heredoc end mark.


1.5.0-nxhtml-1.61
-----------------

* Add `php-mode-to-use`.
* Made underscore part of identifiers.
* Remove `php-mode-to`.
* Make the indentation check only the current line.
* Warn only once per session about indentation.
* Tell if cannot complete in `php-complete-function`.
* Move back point after checking indentation in
  `php-check-html-for-indentation`.
* Add `c-at-vsemi-p-fn`.


1.5
---

* Support function keywords like `public` and `private`.
* Support the ampersand for function-based commands.
* Add support for the follow keywords in Imenu:
  - `abstract`
  - `final`
  - `static`
  - `public`
  - `private`
* Fix the reverse order of Imenu entries.
* Use `font-lock-preprocessor-face` for PHP and ASP tags.
* Make `php-mode-modified` a literal value instead of a computed
  string.
* Add date and time constants from PHP.
* Fix false syntax highlighting of keywords due to the underscore.
* Change HTML indentation warning to match only HTML at the beginning
  of the line.
* Fix byte-compiler warnings.
* Clean-up whitespace and audit style consistency of code.
* Remove conditional bindings and XEmacs code that likely does
  nothing.


1.4
---

* Update to GNU GPL Version 3.
* Port to Emacs 22 (CC Mode 5.31).
* `M-x php-mode-version` shows that version.
* Provide `beginning-of-defun` and `end-of-defun`.
* Support add-log library.
* Fix `__CLASS__` constant.
* Allow Imenu to see visibility declarations.


1,3
---

* Change the definition of `#` to correct highlighting and
  indentation.
* Change the highlighting of HTML.
