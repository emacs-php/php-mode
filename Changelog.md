# Changes for PHP Mode by Version

All notable changes of the PHP Mode 1.19.1 release series are documented in this file using the [Keep a CHANGELOG](http://keepachangelog.com/) principles.

## [1.21.2]

### Fixed

 * Function `php-beginning-of-defun` should return non-nil on success
 ([#503](https://github.com/emacs-php/php-mode/issues/503))

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

[@ejmr]: https://github.com/ejmr
[@fabacino]: https://github.com/fabacino
