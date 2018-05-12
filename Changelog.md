# Changes for PHP Mode by Version

All notable changes of the PHP Mode 1.19.1 release series are documented in this file using the [Keep a CHANGELOG](http://keepachangelog.com/) principles.

## [1.19.1] - 2018-05-12

### Added

 * Add some functions/variables in `php-project` package
    * `php-project-get-php-executable` ([#436](https://github.com/ejmr/php-mode/pull/436))
    * `php-project-get-phan-executable` ([#436](https://github.com/ejmr/php-mode/pull/436))
 * Add `php-debug` command for reporting a bug

### Changed

 * Add highlighting of return types for abstract functions ([#461](https://github.com/ejmr/php-mode/pull/461) by [@fabacino](https://github.com/fabacino))

### Fixed

 * Fix `c-anchored-cpp-prefix` in Emacs 26 ([#453](https://github.com/ejmr/php-mode/pull/453))
 * Fix (re-implemteed) `php-complete-function` ([#454](https://github.com/ejmr/php-mode/pull/454))
 * Fix some problem of `php-set-style` *(disabled on default)*
   * Fix lazy evaluation of php-set-style ([#444](https://github.com/ejmr/php-mode/pull/444))
   * Do not use `php-mode-enable-backup-style-variables` in initialize ([#452](https://github.com/ejmr/php-mode/pull/452))
   * Temporarily disable `php-mode-enable-project-coding-style`  ([#450](https://github.com/ejmr/php-mode/pull/450))
 * Fix heredoc/nowdoc syntax problem ([#440](https://github.com/ejmr/php-mode/pull/440))
 * Delete duplicate font locks with keywords not related to PHP language ([#432](https://github.com/ejmr/php-mode/pull/432))
 * Make `php-project-root` variable accept a string ([#458](https://github.com/ejmr/php-mode/pull/458))

## Before 1.19.0

See [Changelog · ejmr/php-mode Wiki](https://github.com/ejmr/php-mode/wiki/Changelog).
