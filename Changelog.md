Changes for PHP Mode by Version
===============================

**Note:** This document does not list every change from every version,
because when I (Eric James Michael Ritz) took over the project I did
not have a record of all changes available.


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
