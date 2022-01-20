# Changes for PHP Mode by Version

All notable changes of the PHP Mode 1.19.1 release series are documented in this file using the [Keep a CHANGELOG](https://keepachangelog.com/) principles.

## Unreleased

### Added

 * Support new PHP 8.0 and 8.1 syntax hilighting and indentation
    * [8.0] `#[Attributes]`
    * [8.1] `readonly` property ([#680])
 * Add `php-imenu-generic-expression-default` for default value or `php-imenu-generic-expression`
   * Add `php-imenu-generic-expression-legacy` for compatibility
   * Add `php-imenu-generic-expression-simple` for simple display
 * Add `php-project-project-find-function` compatible with `project-find-functions`

### Changed

 * Optimized propertize process ([#669])
   * Reimoplement `php-syntax-propertize-function` using `syntax-propertize-rules`
   * Make propertize PHP 8 `#[Attribute]` always enabled
   * Changed grouping of `php-heredoc-start-re`
 * Re-organized `php-imenu-generic-expression`
   * Added `Import`, `Constants` and `Properties`
   * Removed `Anonymous Functions`
   * Renamed `Named Functions` to `Functions`
   * Renamed `All Methods` to `Methods`
   * Removed `Public Methods`, `Protected Methods` and `Provate Methods`
   * Unified `Classes`, `Traits`, `Interfaces` into `Classes`

[#669]: https://github.com/emacs-php/php-mode/pull/669
[#680]: https://github.com/emacs-php/php-mode/pull/680

## [1.24.0] - 2021-03-07

### Added

 * Support new PHP 8.0 and 8.1 syntax hilighting and indentation
   * [8.0] `match` expression ([#632])
   * [8.0] `mixed` pseudo type declaration ([#633])
   * [8.1] `enum` statement ([#653])

### Changed

 * Drop support for Emacs 24 and 25.1 ([#654])
 * Raises an error when the byte-compiled Cc Mode version does not match that version at runtime.
   * You will need to recompile php-mode after the GNU Emacs version changes.

## Removed

 * Remove `(require 'add-log)`

### Fixed

 * Fix font-lock highlighting
   * `!=` ([#630])
 * Fix Heredoc and Nowdoc highlighting ([#651] thanks [@antoineB])
 * Fix documentation mistake
   * `php-mode-lineup-cascaded-calls` in README.md ([#644] thanks [@cweiske]!)

[#630]: https://github.com/emacs-php/php-mode/pull/630
[#632]: https://github.com/emacs-php/php-mode/pull/632
[#633]: https://github.com/emacs-php/php-mode/pull/633
[#644]: https://github.com/emacs-php/php-mode/pull/644
[#651]: https://github.com/emacs-php/php-mode/pull/651
[#653]: https://github.com/emacs-php/php-mode/pull/653
[#654]: https://github.com/emacs-php/php-mode/pull/654
[@antoineB]: https://github.com/antoineB
[@cweiske]: https://github.com/cweiske

## [1.23.0] - 2020-05-06

Initial support for PHP 8 has been added.  PHP Mode has some issues for compatibility with Emacs 27.0.

The list of all past contributors has been moved to [`AUTHORS.md`](/AUTHORS.md).

This release is positioned as the last minor version of the PHP Mode 1.x series.

### Added

 * Add `php-project-use-projectile-to-detect-root` ([#608])
 * Add PHP file extensions to `auto-mode-alist` ([#609])
   * `.php.inc` is file extension for [Rector](https://github.com/rectorphp/rector)'s test code.
   * `.stub` is file extension for [PHPStan](https://github.com/phpstan/phpstan)'s stub file.
 * Add `php-mode-disable-c-auto-align-backslashes` ([#621])
 * Add PHP 8.0 Attribute syntax as vsemi ([#626])
 * Add feature `php-align` from [tetsujin/emacs-php-align] ([#615], [melpa/melpa#6759])

### Changed

 * Disable `c-auto-align-backslashes` by default. ([#621])
 * Add new faces and font locking ([#611], [#614])

### Removed

 * Remove `php-mode-extra-constants` variable and function. ([#605])
   * This mechanism is for synchronizing the added user-defined constants with WebMode, but it hasn't worked since 2014.

### Fixed

 * Fix indentation of object operator (->) at the beginning of line ([#623], [#624])

[#605]: https://github.com/emacs-php/php-mode/pull/605
[#608]: https://github.com/emacs-php/php-mode/pull/608
[#609]: https://github.com/emacs-php/php-mode/pull/609
[#611]: https://github.com/emacs-php/php-mode/pull/611
[#614]: https://github.com/emacs-php/php-mode/pull/614
[#615]: https://github.com/emacs-php/php-mode/pull/615
[#621]: https://github.com/emacs-php/php-mode/pull/621
[#623]: https://github.com/emacs-php/php-mode/issues/623
[#624]: https://github.com/emacs-php/php-mode/pull/624
[melpa/melpa#6759]: https://github.com/melpa/melpa/pull/6759
[tetsujin/emacs-php-align]: https://github.com/tetsujin/emacs-php-align

## [1.22.2] - 2019-12-23

A face has been added for coloring PHP syntax. Thank you [@minikN]!

This release is a minor modified version of the 1.22.x series.
PHP Mode 2.0 is planned to be released in January 2020.

### Added

 * Add `php-project-etags-file` and `php-project-apply-local-variables` ([#591])
 * Add `php-find-system-php-ini-file` and `php-ini` command ([#593])

### Changed

 * Improve PHP syntax highlighting (by [@minikN])
   * Add support for multiple operators ([#594])
   * Add `=>` to assignment operators ([#602], [#603])
 * Plain faces no longer inherit `default` ([#597])

### Removed

 * Remove `php-mode-modified` variable ([#590])

### Fixed

 * Fix anonymous class indentation ([#598])

[@minikN]: https://github.com/minikN
[#590]: https://github.com/emacs-php/php-mode/pull/590
[#591]: https://github.com/emacs-php/php-mode/pull/591
[#593]: https://github.com/emacs-php/php-mode/pull/593
[#594]: https://github.com/emacs-php/php-mode/pull/594
[#597]: https://github.com/emacs-php/php-mode/pull/597
[#598]: https://github.com/emacs-php/php-mode/pull/598
[#602]: https://github.com/emacs-php/php-mode/pull/602
[#603]: https://github.com/emacs-php/php-mode/pull/603

## [1.22.1] - 2019-11-10

This release is a minor modified version of the 1.22.x series.
PHP Mode 2.0 is planned to be released in January 2020.

### Fixed

 * Fix usas of `rx-form` ([#580], thank you [@cmack])
 * Fix `php-mode-coding-style` custom variable ([#581])
 * Fix unexpected indentation of (return typed) closures written in arguments ([#585])

[@cmack]: https://github.com/cmack
[#580]: https://github.com/emacs-php/php-mode/pull/580
[#581]: https://github.com/emacs-php/php-mode/pull/581
[#585]: https://github.com/emacs-php/php-mode/pull/585

## [1.22.0] - 2019-09-27

~~This release is positioned as the last minor version of the PHP Mode 1.x series.~~
PHP Mode 2.0 is planned to be released in January 2020.

### Added

 * Improve PHP 7 and PHPDoc support
   * Highlighting typed property ([#545])
   * Highlighting `{@inheritdoc}` tag ([#566])
   * Highlighting  multiple `catch`  ([#567])
 * Add new `php-class` face ([#545])
 * Add new `php-run-builtin-web-server` command for invoke `php -S` buitin server ([#548])
 * Add new `php-copyit-fqsen` command for kill current method FQSEN ([#561])
 * Apply lineup cascaded call (method chain separated by new line) for all styles ([#563], [#572])

### Changed

 * Loose HTML template detection algorithm ([#558])
 * Move php-mode-test.el into tests directory ([#559])
   * `php-mode-autoloads.el` no longer includes `php-mode-test`
 * Modify syntax-entry `$` to `"_"` from `"'"` ([#565])
 * Make `php-set-style` display only styles that inherited "php" ([#573])

### Fixed

 * Fix default major mode for `*.phpt` file as `php-default-major-mode` ([#542])
 * prevent recursion in `php-syntax-propertize-extend-region` ([$556], thanks [Herbert Jones][@herbertjones])
 * Fix call `run-hooks` in `set-style` ([#571])

[#542]: https://github.com/emacs-php/php-mode/pull/542
[#545]: https://github.com/emacs-php/php-mode/pull/545
[#548]: https://github.com/emacs-php/php-mode/pull/548
[#556]: https://github.com/emacs-php/php-mode/pull/556
[#558]: https://github.com/emacs-php/php-mode/pull/558
[#559]: https://github.com/emacs-php/php-mode/pull/559
[#561]: https://github.com/emacs-php/php-mode/pull/561
[#563]: https://github.com/emacs-php/php-mode/issues/563
[#565]: https://github.com/emacs-php/php-mode/pull/565
[#566]: https://github.com/emacs-php/php-mode/pull/566
[#567]: https://github.com/emacs-php/php-mode/pull/567
[#571]: https://github.com/emacs-php/php-mode/pull/571
[#572]: https://github.com/emacs-php/php-mode/pull/572
[#572]: https://github.com/emacs-php/php-mode/pull/572
[#573]: https://github.com/emacs-php/php-mode/pull/573
[#556]: https://github.com/emacs-php/php-mode/pull/556
[@herbertjones]: https://github.com/herbertjones

## [1.21.4] - 2019-05-29

This version contains unobtrusive changes for future compatibility. Also, `forward-page` /` backward-page` may improve your operation. Thank you [@takeokunn].

### Added

 * A new command `php-mode-maybe` has been added to improve the compatibility of Blade templates and HTML templates with Web Mode. ([#532])
 * A new custom variable php-mode-page-delimiter is added, which is assigned to page-delimiter in PHP-Mode. It allows you to move smoothly to the previous and subsequent definition statements with `forward-page` (<kbd>C-x C-[</kbd>) and `backward-page` (<kbd>C-x C-]</kbd>). ([#540] by [@takeokunn])

### Changed

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
