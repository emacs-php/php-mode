# Changes for PHP Mode by Version

All notable changes of the PHP Mode 1.19.1 release series are documented in this file using the [Keep a CHANGELOG](http://keepachangelog.com/) principles.

## [1.21.4] - 2019-05-29

This version contains unobtrusive changes for future compatibility. Also, `forward-page` /` backward-page` may improve your operation. Thank you [@takeokunn].

### Added

 * A new command `php-mode-maybe` has been added to improve the compatibility of Blade templates and HTML templates with Web Mode. ([#532])
 * A new custom variable php-mode-page-delimiter is added, which is assigned to page-delimiter in PHP-Mode. It allows you to move smoothly to the previous and subsequent definition statements with `forward-page` (<kbd>C-x C-[</kbd>) and `backward-page` (<kbd>C-x C-]</kbd>). ([#540] by [@takeokunn])

### Change

 * `php-maybe-mode` and `.php` files are associated with `auto-mode-alist` instead of `php-mode` ([#532])
 * Compatibility with [poly-php] has been improved. This has not yet been officially released.

### Deprecated

 * PHP Mode does not support phpt files . Consider installing the [phpt-mode] package. This mode is a wrapper around Polymode based PHP-Mode.

[#532]: https://github.com/emacs-php/php-mode/pull/532
[#539]: https://github.com/emacs-php/php-mode/pull/539
[#540]: https://github.com/emacs-php/php-mode/pull/540
[@takeokunn]: https://github.com/takeokunn

## [1.21.3] - 2019-05-25

This version includes both PHP syntax support enhancements and performance improvements. See https://github.com/emacs-php/php-mode/projects/1 for all issues for this release.  Also, [@mallt] has helped me improve it with a number of issues. thank you very much.

### Added

 * Added new Faces to display PHP syntax
   * `php-constant-assign`
   * `php-magical-constant`
   * `php-errorcontrol-op`
 * Some funtions have been split into features such as `php.el`, `php-face.el` and `php-mode-debug.el`.

### Fixed

 * Improved performance when opening large files ([#491], [#531])

[#491]: https://github.com/emacs-php/php-mode/issues/491
[#531]: https://github.com/emacs-php/php-mode/pull/531

### Changed

 * Syntax highlighting has been adjusted

### Removed

 * `php-ext` and Skeleton templates have been split into [php-skeleton] ([#534])

[php-skeleton]: https://github.com/emacs-php/php-skeleton
[#534]: https://github.com/emacs-php/php-mode/pull/534

## [1.21.2] - 2019-05-11

It officially supports **PHP 7.3** and **Emacs 26.2**.
Many improvements have been received from [@sergeyklay], thank you!

### Added

 * Highlighting added `fn` keyword supported by [PHP 7.2 arrow function] ([#506])

### Fixed

 * Function `php-beginning-of-defun` should return non-nil on success ([#503])
 * Fixed an error that occurred in some heredoc/nowdoc ([#496])

### Changed

 * Support PHP 7.3 heredoc/nowdoc ([#496])
 * Minor optimization of font-lock regular expression ([#510])

## [1.21.1] - 2019-04-01

### Changed

 * Fixed an error that occurred in Emacs 27
 * Modify URL of PHP web site
 * Fix highlighting of static method calls

## [1.21.0] - 2019-01-09

A minor change from the previous version, but this version has BC brake about `c-mode-hook`.

### Added

 * Add `php` customize group.
   * This group is positioned above `php-mode`.
   * This group was originally planned to be included in v1.20.0.
 * Add `php-mode-disable-c-mode-hook` custom variable.
   * Setting this variable to `nil` will restore the behavior of `c-mode-hook` up to the previous version.

### Changed

 * `php-mode` initialization no longer executes hooks for `c-mode`.
   *  If you want to return to the same behavior as the conventional PHP-Mode, set `nil` to `php-mode-disable-c-mode-hook`.

## [1.20.0] - 2018-12-07

Start preparing for major refactoring in major mode.

### Added

 * Add php-project variables for workflow ([#488](https://github.com/emacs-php/php-mode/pull/488))
    * The following variables are reserved as file/directory local variables.
      * `php-project-repl`
      * `php-project-unit-test`
      * `php-project-deploy`
      * `php-project-build`
      * `php-project-server-start`

### Changed

 * Improve code highlight
    * Fix highlighting of callable keyword ([#471](https://github.com/emacs-php/php-mode/pull/471) by [@fabacino])
    * Highlight the `?` character for nullable type hints and return types ([#482](https://github.com/emacs-php/php-mode/pull/482) by [@fabacino])
 * Modify "customize group" and variable names
    * Add php-mode group and some variables belong to new group ([#486](https://github.com/emacs-php/php-mode/pull/486))
      * `php-default-face` → `php-mode-default-face`
      * `php-speedbar-config` → `'php-mode-speedbar-config`
      * `php-template-compatibility` → `php-mode-template-compatibility`
      * `php-lineup-cascaded-calls` → `php-mode-lineup-cascaded-calls`
      * `php-extra-constants` → `php-mode-extra-constants`
      * `php-do-not-use-semantic-imenu` → `php-mode-do-not-use-semantic-imenu`
 * Modify documents and copyright
    * Move the URL of Website to https://github.com/emacs-php/php-mode
    * Add copyright notation about [Friends of Emacs-PHP development](https://github.com/emacs-php)
    * Add a "Hall of Fame" to the contributors list ([#481](https://github.com/emacs-php/php-mode/pull/481) by [@ejmr] and thanks [@sergey48k](https://github.com/sergey48k) and [@sourcerer-io](https://github.com/sourcerer-io) project)
 * Some refactors
    * Fix style and suppress warning in compile [#485](https://github.com/emacs-php/php-mode/pull/485)

<!--
 * Divide `php-mode.el` into `php.el` and `php-helper.el` for refactor
   *  With this change, we provide simple functions to other packages without loading `php-mode`.
-->

## [1.19.1] - 2018-05-12

### Added

 * Add some functions/variables in `php-project` package
    * `php-project-get-php-executable` ([#436](https://github.com/emacs-php/php-mode/pull/436))
    * `php-project-get-phan-executable` ([#436](https://github.com/emacs-php/php-mode/pull/436))
 * Add `php-debug` command for reporting a bug

### Changed

 * Add highlighting of return types for abstract functions ([#461](https://github.com/emacs-php/php-mode/pull/461) by [@fabacino](https://github.com/fabacino))

### Fixed

 * Fix `c-anchored-cpp-prefix` in Emacs 26 ([#453](https://github.com/emacs-php/php-mode/pull/453))
 * Fix (re-implemteed) `php-complete-function` ([#454](https://github.com/emacs-php/php-mode/pull/454))
 * Fix some problem of `php-set-style` *(disabled on default)*
   * Fix lazy evaluation of php-set-style ([#444](https://github.com/emacs-php/php-mode/pull/444))
   * Do not use `php-mode-enable-backup-style-variables` in initialize ([#452](https://github.com/emacs-php/php-mode/pull/452))
   * Temporarily disable `php-mode-enable-project-coding-style`  ([#450](https://github.com/emacs-php/php-mode/pull/450))
 * Fix heredoc/nowdoc syntax problem ([#440](https://github.com/emacs-php/php-mode/pull/440))
 * Delete duplicate font locks with keywords not related to PHP language ([#432](https://github.com/emacs-php/php-mode/pull/432))
 * Make `php-project-root` variable accept a string ([#458](https://github.com/emacs-php/php-mode/pull/458))

## Before 1.19.0

See [Changelog · emacs-php/php-mode Wiki](https://github.com/emacs-php/php-mode/wiki/Changelog).

[#496]: https://github.com/emacs-php/php-mode/pull/496
[#503]: https://github.com/emacs-php/php-mode/issues/503
[#506]: https://github.com/emacs-php/php-mode/issues/506
[#510]: https://github.com/emacs-php/php-mode/pull/510
[@ejmr]: https://github.com/ejmr
[@fabacino]: https://github.com/fabacino
[@mallt]: https://github.com/mallt
[@sergeyklay]: https://github.com/sergeyklay
[PHP 7.2 arrow function]: https://wiki.php.net/rfc/arrow_functions_v2
[poly-php]: https://github.com/emacs-php/poly-php
[phpt-mode]: https://github.com/emacs-php/phpt-mode
