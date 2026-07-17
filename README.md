<div align="center">
  <h1>Emacs PHP Mode</h1>

[![Emacs: 30.0](https://img.shields.io/badge/Emacs-30.0-blue.svg)](https://www.gnu.org/software/emacs/)
[![lang: PHP 8.4](https://img.shields.io/badge/lang-PHP%208.4-brightgreen.svg)](https://www.php.net/releases/8.4/)
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

> [!IMPORTANT]
> PHP Mode has been rewritten and no longer depends on CC Mode.  Indentation is now handled by an engine derived from `js.el`, which is bundled with GNU Emacs.  The previous CC Mode based implementation is still available as `php-cc-mode`; see [Legacy CC Mode implementation](#legacy-cc-mode-implementation-php-cc-mode).

> [!WARNING]
> After upgrading Emacs, when you open a PHP file for the first time, you may encounter errors related to CC Mode. These errors occur because a previously byte-compiled version of PHP Mode, cached on your disk, differs from the newly installed one. Reinstalling PHP Mode should resolve the issue.
>
> Try running **`M-x php-mode-debug-reinstall`** or **`M-x package-reinstall php-mode`**.

[releases]: https://github.com/emacs-php/php-mode/releases
[discussions-emacs30]: https://github.com/emacs-php/php-mode/discussions/798

## Installation

**PHP Mode works with Emacs 28.1 or later** ([#811]). For details on supported versions, see [Supported Version].
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

### Coding Styles

`php-mode` sets indentation and related editing variables according to `php-mode-coding-style`.  The default style is `per` ([PER Coding Style 2.0][per-cs]); previous versions defaulted to `pear`.  The available styles are `per`, `psr2`, `pear`, `drupal` and `wordpress`.

The `symfony2` style has been removed, since PER supersedes the coding style used by modern Symfony.  It remains available in `php-cc-mode`.

Because the indentation engine no longer uses CC Mode, `c-basic-offset` is obsolete in `php-mode`; customize `php-indent-offset` instead.  For backward compatibility, when a project sets `c-basic-offset` buffer-locally (for example via `.dir-locals.el` or file-local variables), `php-mode` copies that value into `php-indent-offset` and displays a warning.

## Completion

`php-complete.el` provides a few small, dependency-light `completion-at-point` functions (capfs) for use without a language server.  Each is usable both as an `M-x` command and as a building block for [`cape`][cape]'s `cape-capf-super`:

- `php-complete-complete-function` — built-in function names.
- `php-complete-complete-path` — a filesystem path inside the `__DIR__ . '/...'` idiom, completed one component at a time and rooted at the directory of the current file (what `__DIR__` resolves to at runtime).

```elisp
(add-hook 'php-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions
                      #'php-complete-complete-path nil t)))

;; …or compose several offline sources into one super-capf with cape:
(add-hook 'php-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions
                      (cape-capf-super #'php-complete-complete-function
                                       #'php-complete-complete-path)
                      nil t)))
```

### A context-sensitive `.` key

Inserting the `. '/'` that bridges `__DIR__` into path completion is deliberately *not* the capf's job; it is left to your editing setup.  `php-dot-context` is the primitive for that: it reports whether point is inside a string or comment (`string-or-comment`), directly after a string literal or a magic constant such as `__DIR__` (`next-to-string`), or in plain code (`code`).  Because the capf and this primitive share the same notion of "string" and "magic constant", key-driven insertion and completion stay consistent.

For example, with [smartchr][smartchr] the `.` key can cycle `->` / `.` / `. ` in code, insert a literal `.` inside strings, and prefer `. ` right after `__DIR__` (which then flows straight into `php-complete-complete-path`):

```elisp
(defun my-php-smartchr-dot (code within-string next-to-string)
  "Build a smartchr for the `.' key using `php-dot-context'."
  (let ((select (lambda ()
                  (pcase (php-dot-context)
                    ('string-or-comment within-string)
                    ('next-to-string    next-to-string)
                    (_                  code)))))
    (smartchr-make-struct
     :cleanup-fn (lambda () (delete-char (- (length (funcall select)))))
     :insert-fn  (lambda () (insert (funcall select))))))

;; (smartchr (my-php-smartchr-dot "->" "." ". ")
;;           (my-php-smartchr-dot ". " ".." "..")
;;           "...")
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

## Legacy CC Mode implementation (php-cc-mode)

Until now, PHP Mode was implemented on top of Emacs' built-in CC Mode.  That implementation is preserved as `php-cc-mode` and is in frozen maintenance: only regression fixes are accepted, and new development happens in the CC Mode independent `php-mode`.  To open a file with the legacy implementation, run `M-x php-cc-mode` or add an entry to `auto-mode-alist` yourself.

So that existing configurations keep working, `php-cc-mode` runs `php-mode-hook` in addition to its own `php-cc-mode-hook`.  Settings described in [the CC Mode manual][cc-mode-manual] apply only to `php-cc-mode`; see the commentary at the top of `lisp/php-cc-mode.el` for details.

## Reporting Bugs

When reporting a bug, please run `M-x php-mode-debug` and include its output in your bug report.  This helps us reproduce any issues you may be experiencing.

## How to Contribute

Please see [CONTRIBUTING.md](CONTRIBUTING.md#english).

## Copyright

PHP Mode is licensed under [GNU General Public License Version 3][gpl-v3] (GPLv3).

This project originated in `php-mode.el` written by [Turadg Aleahmad][@turadg] in 1999.  In 2013 [Daniel Hackney][@haxney] began rewriting parts of PHP Mode in terms of Emacs' built-in CC Mode.  In 2026 the mode was rewritten again to remove the CC Mode dependency.  Other contributors are listed in [Authors] and [Contributors].

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
[cape]: https://github.com/minad/cape
[cc-mode-manual]: https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
[gpl-v3]: https://www.gnu.org/licenses/gpl-3.0
[smartchr]: https://github.com/imakado/emacs-smartchr
[per-cs]: https://www.php-fig.org/per/coding-style/
[#811]: https://github.com/emacs-php/php-mode/issues/811
[nongnu-elpa-badge]: https://elpa.nongnu.org/nongnu/php-mode.svg
[nongnu-elpa]: https://elpa.nongnu.org/nongnu/php-mode.html
[melpa-badge]: http://melpa.org/packages/php-mode-badge.svg
[melpa-getting-started]: https://melpa.org/#/getting-started
[melpa-link]: http://melpa.org/#/php-mode
[php-mode]: https://github.com/emacs-php/php-mode
[wiki]: https://github.com/emacs-php/php-mode/wiki
[wiki-manual-installation]: https://github.com/emacs-php/php-mode/wiki/Manual-installation
