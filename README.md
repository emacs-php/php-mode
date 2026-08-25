<div align="center">
  <h1>Emacs PHP Mode</h1>

[![Emacs: 31.1](https://img.shields.io/badge/Emacs-31.1-blue.svg)](https://www.gnu.org/software/emacs/)
[![lang: PHP 8.5](https://img.shields.io/badge/lang-PHP%208.5-brightgreen.svg)](https://www.php.net/releases/8.5/)
[![Build Status](https://github.com/emacs-php/php-mode/workflows/CI/badge.svg)](https://github.com/emacs-php/php-mode/actions)
[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)][gpl-v3]<br>
[![NonGNU ELPA][nongnu-elpa-badge]][nongnu-elpa]
[![melpa badge][melpa-badge]][melpa-link]

A powerful and flexible Emacs major mode for editing PHP scripts

English &nbsp;&nbsp;|&nbsp;&nbsp; [日本語](README.ja.md)

</div>

Please submit any bug reports or feature requests by creating issues on [the GitHub page for PHP Mode][php-mode].

> [!NOTE]
> The [latest version][releases] of PHP Mode supports Emacs 30.  
> Please feel free to [open a discussion][discussions-emacs30] if you have any issues upgrading to Emacs 30.

> [!WARNING]
> After upgrading Emacs, when you open a PHP file for the first time, you may encounter errors related to CC Mode. These errors occur because a previously byte-compiled version of PHP Mode, cached on your disk, differs from the newly installed one. Reinstalling PHP Mode should resolve the issue.
>
> Try running **`M-x php-mode-debug-reinstall`** or **`M-x package-reinstall php-mode`**.

[releases]: https://github.com/emacs-php/php-mode/releases
[discussions-emacs30]: https://github.com/emacs-php/php-mode/discussions/798

## Installation

**PHP Mode works with Emacs 27.1 or later.** For details on supported versions, see [Supported Version].
On Emacs 28 or later, you can install it simply by running:

```
M-x package-install php-mode
```

By [adding MELPA to `package-archives`][melpa-getting-started], you can extend Emacs with numerous packages from the web.

If you prefer not to rely on a package manager, you can install the Lisp files directly in the traditional manner.  See [Manual installation][wiki-manual-installation] for our recommended method.

## Configuration

### Personal Settings

You can add configurations for PHP Mode in your `.emacs` file (`~/.emacs.d/init.el`):

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

You can add project-specific settings by creating a `.dir-locals.el` or `.dir-locals-2.el` file in the project's root directory.  It is recommended not to put these files under version control, as they depend on the packages installed on each user's Emacs.

```lisp
((nil
  (php-project-root . git)
  (php-project-coding-style . psr2)))
```

### PHP-IDE: integrating LSP clients and Phpactor

`php-ide` (`php-ide.el`) is an **experimental** bridge between PHP Mode and IDE-like tools: [Eglot](https://github.com/joaotavora/eglot), [lsp-mode](https://github.com/emacs-lsp/lsp-mode), [lsp-bridge](https://github.com/manateelazycat/lsp-bridge), and [Phpactor](https://github.com/emacs-php/phpactor.el).  It does not implement any of these features itself — it only activates or deactivates whichever one(s) you choose, through a single `php-ide-mode` minor mode. See the Commentary at the top of `php-ide.el` for the full reference.

```lisp
(defun my-php-mode-init ()
  (add-hook 'hack-local-variables-hook 'php-ide-turn-on nil t))

(with-eval-after-load 'php-ide
  (custom-set-variables
   '(php-ide-features '(eglot))              ;; and/or '(phpactor), '(lsp-mode), '(lsp-bridge)
   '(php-ide-eglot-executable 'intelephense)  ;; or 'phpantom, 'php-lsp, 'phpactor, a path string, or a list of strings
   '(php-ide-mode-lighter "")))               ;; hide PHP-IDE from the mode line
```

`php-ide-turn-on` does nothing when `php-ide-features` is unset, so it is safe to add unconditionally as above — PHP-IDE stays off until a feature is configured, either globally as above or per project below.

Useful commands once `php-ide` is loaded:

* `M-x php-ide-mode` — toggle PHP-IDE for the current buffer, using `php-ide-features`.
* `M-x php-ide-turn-on` — same, but never errors when `php-ide-features` is unset.
* `M-x php-ide-set-feature` — pick, interactively, one of the features actually available on this system (i.e. whose backing package is installed) and enable it for the current buffer.
* `M-x php-ide-status` — report whether PHP-IDE is on, what is configured, and what is available.

#### Per-project PHP-IDE configuration

```lisp
((nil (php-project-root . git)
      (php-ide-features . (eglot))))
```

`php-ide-features` and `php-ide-eglot-executable` are only treated as safe for `.dir-locals.el` when they name one of PHP-IDE's own built-in features or bundled executable presets (e.g. `intelephense`, `phpantom`, `php-lsp`, `phpactor`).  Anything else — a raw executable path, explicit command arguments, or a custom `php-ide-mode-functions` hook — still goes through Emacs's normal confirmation for risky directory-local variables, since applying those silently would let any repository run an arbitrary command (or Lisp function) in your Emacs just by having you open a file in it.

### Integration with `project.el` and Projectile

`php-project-get-root-dir` first looks for a PHP-specific marker (`.projectile`, `composer.json`/`composer.lock`, then a VCS directory).  Preferring `composer.json` over the VCS root matters in monorepos, where per-package `vendor/autoload.php` and coding styles are what `php-mode` cares about.  When none of those markers is found, it now falls back to `project-current`, so any [`project.el`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html) backend can contribute detection:

* **Projectile 3** registers itself on `project-find-functions` when `projectile-mode` is enabled, so its detection is picked up automatically.  The old `php-project-use-projectile-to-detect-root` option is therefore obsolete.
* **Extra root markers** can be declared without Projectile via `project-vc-extra-root-markers` (Emacs 29+), for example in `.dir-locals.el`:

  ```lisp
  ((nil
    (project-vc-extra-root-markers . ("composer.json"))))
  ```

Conversely, to expose PHP-specific detection (the `composer.json`-first precedence above) to `project.el` consumers such as Eglot or `project-find-file`, register `php-project-project-find-function` with a low priority so it only supplements the built-in VC detection:

```lisp
(add-hook 'project-find-functions #'php-project-project-find-function 90)
```

## Editing files that mix HTML and PHP

`php-mode` is designed for pure PHP scripts.  Files that embed PHP inside HTML, such as templates, are better edited in a major mode that understands both languages.  Indentation in particular is unreliable when the HTML part of a file is edited in plain `php-mode`.

For such files, PHP Mode defers to `php-html-template-major-mode`, which defaults to [`web-mode`](https://web-mode.org/).  Set it to any mode you prefer:

```lisp
(setopt php-html-template-major-mode 'web-mode)
```

### How the major mode is chosen

Files with a `.php` extension are opened through `php-mode-maybe`, which picks the major mode from the file name and its content:

- File names matching `php-template-mode-alist` (for example `.phtml` and `.blade.php`) open in the matching template mode.
- Otherwise the choice follows `php-project-php-file-as-template`, a directory-local variable:
  - `auto` (default): switch to `php-html-template-major-mode` when the file contains an HTML tag.
  - `t`: treat every `.php` file in the directory as a template.
  - `nil`: treat every `.php` file as a plain PHP script.
- When nothing else applies, the file opens in `php-default-major-mode` (`php-mode`).

Set `php-project-php-file-as-template` per project in `.dir-locals.el`:

```lisp
((nil
  (php-project-php-file-as-template . nil)))
```

### Switching away from php-mode

If you are already in `php-mode` and indent a file that contains HTML tags, PHP Mode warns you and offers to switch to `php-html-template-major-mode`.  Set `php-mode-warn-if-html-template` to `nil` to turn off this prompt.

## Reporting Bugs

When reporting a bug, please run `M-x php-mode-debug` and include its output in your bug report.  This helps us reproduce any issues you may be experiencing.

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
