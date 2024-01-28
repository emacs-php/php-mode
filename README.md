<div align="center">
  <h1>Emacs PHP Mode</h1>

[![Emacs: 29.2](https://img.shields.io/badge/Emacs-29.2-blue.svg)](https://www.gnu.org/software/emacs/)
[![lang: PHP 8.3](https://img.shields.io/badge/lang-PHP%208.3-brightgreen.svg)](https://www.php.net/manual/migration83.php)
[![lang: PHP 7](https://img.shields.io/badge/lang-PHP%207-green.svg)](https://www.php.net/downloads.php)
[![Build Status](https://github.com/emacs-php/php-mode/workflows/CI/badge.svg)](https://github.com/emacs-php/php-mode/actions)
[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)][gpl-v3]<br>
[![NonGNU ELPA][nongnu-elpa-badge]][nongnu-elpa]
[![melpa badge][melpa-badge]][melpa-link]

A powerful and flexible Emacs major mode for editing PHP scripts

English &nbsp;&nbsp;|&nbsp;&nbsp; [日本語](README.ja.md)

</div>

Please submit any bug reports or feature requests by creating issues on [the GitHub page for PHP Mode][php-mode].

> [!NOTE]
> The [latest version][releases] of PHP Mode supports Emacs 29. <br />Please feel free to [write to disucuss][disscussions-emacs29] if you have problems upgrading to Emacs 29.

[releases]: https://github.com/emacs-php/php-mode/releases
[disscussions-emacs29]: https://github.com/emacs-php/php-mode/discussions/751

## Installation

**PHP Mode works with Emacs 26.1 or later**. For details on supported versions, see [Supported Version]. Emacs 28 or later can be installed simply by running the following command.

```
M-x package-install php-mode
```

By [adding MELPA to `package-archives`][melpa-getting-started], you can extend Emacs with many packages from the web.

If you don't want to depend on a package manager, you can install Lisp files directly in the traditional way.  See [Manual installation][wiki-manual-installation] for our recommended setup method.

## Configuration

### Personal Settings

You can add configurations for PHP mode in the .emacs file (`~/.emacs.d/init.el`):

```lisp
(defun my-php-mode-init ()
  (subword-mode 1)
  (setq-local show-trailing-whitespace t)
  (setq-local ac-disable-faces '(font-lock-comment-face font-lock-string-face))
  (add-hook 'hack-local-variables-hook 'php-ide-turn-on nil t))

(with-eval-after-load 'php-mode
  (add-hook 'php-mode-hook #'my-php-mode-init)
  (custom-set-variables
   '(php-mode-coding-style 'psr2)
   '(php-mode-template-compatibility nil)
   '(php-imenu-generic-expression 'php-imenu-generic-expression-simple))

  ;; If you find phpcs to be bothersome, you can disable it.
  (when (require 'flycheck nil)
    (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
    (add-to-list 'flycheck-disabled-checkers 'php-phpcs)))
```

### Project Setting

You can add project-specific settings by creating a `.dir-locals.el` or `.dir-locals-2.el` file in the project's root directory.  It is recommended not to put these files under version control, as they depend on the packages installed in each user's Emacs.

```lisp
((nil
  (php-project-root . git)
  (php-project-coding-style . psr2)))
```

## Reporting Bugs

When reporting a bug please run the function `M-x php-mode-debug` and include its output in your bug report.  This helps up reproduce any problem you may have.

## How to Contribute

Please see [CONTRIBUTING.md](CONTRIBUTING.md#english).


## Copyright

PHP Mode is licensed under [GNU General Public License Version 3][gpl-v3] (GPLv3).

This project originated in `php-mode.el` written by [Turadg Aleahmad][@turadg] in 1999.  In 2013 [Daniel Hackney][@haxney] began rewriting parts of PHP Mode in terms of Emacs' built-in CC Mode.  Other contributors are listed in [Authors] and [Contributors].

This project was maintained by [Eric James Michael Ritz][@ejmr] until 2017. Currently, the [Friends of Emacs-PHP Development][@emacs-php] community inherits PHP Mode.

> ```
> Copyright (C) 2022  Friends of Emacs-PHP development
> Copyright (C) 1999, 2000, 2001, 2003, 2004 Turadg Aleahmad
>               2008 Aaron S. Hawley
>               2011, 2012, 2013, 2014, 2015, 2016, 2017 Eric James Michael Ritz
> ```
>
> This program is free software; you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or
> (at your option) any later version.
>
> This program is distributed in the hope that it will be useful,
> but WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
> GNU General Public License for more details.
>
> You should have received a copy of the GNU General Public License
> along with this program.  If not, see <https://www.gnu.org/licenses/>.

[@ejmr]: https://github.com/ejmr
[@emacs-php]: https://github.com/emacs-php
[@haxney]: https://github.com/haxney
[@turadg]: https://github.com/turadg
[Authors]: https://github.com/emacs-php/php-mode/wiki/Authors
[Contributors]: https://github.com/emacs-php/php-mode/graphs/contributors
[Supported Version]: https://github.com/emacs-php/php-mode/wiki/Supported-Version
[gpl-v3]: https://www.gnu.org/licenses/gpl-3.0
[nongnu-elpa-badge]: https://elpa.nongnu.org/nongnu/php-mode.svg
[nongnu-elpa]: https://elpa.nongnu.org/nongnu/php-mode.html
[melpa-badge]: http://melpa.org/packages/php-mode-badge.svg
[melpa-getting-started]: https://melpa.org/#/getting-started
[melpa-link]: http://melpa.org/#/php-mode
[php-mode]: https://github.com/emacs-php/php-mode
[wiki]: https://github.com/emacs-php/php-mode/wiki
[wiki-manual-installation]: https://github.com/emacs-php/php-mode/wiki/Manual-installation
