<div align="center">
  <h1>Emacs PHP Mode</h1>

[![Emacs: 29.2](https://img.shields.io/badge/Emacs-29.2-blue.svg)](https://www.gnu.org/software/emacs/)
[![lang: PHP 8.3](https://img.shields.io/badge/lang-PHP%208.3-brightgreen.svg)](https://php.net/manual/migration83.php)
[![lang: PHP 7](https://img.shields.io/badge/lang-PHP%207-green.svg)](https://php.net/downloads.php)
[![Build Status](https://github.com/emacs-php/php-mode/workflows/CI/badge.svg)](https://github.com/emacs-php/php-mode/actions)
[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)][gpl-v3]<br>
[![NonGNU ELPA][nongnu-elpa-badge]][nongnu-elpa]
[![melpa badge][melpa-badge]][melpa-link]

A powerful and flexible Emacs major mode for editing PHP scripts

[English](README.md) &nbsp;&nbsp;|&nbsp;&nbsp; 日本語

</div>

[GitHubプロジェクト][php-mode]にissueを作成してバグ報告や機能リクエストを送ってください。

> [!NOTE]
> [最新版][releases]のPHP ModeはEmacs 29をサポートしています。<br />アップグレードに伴うトラブルは[Discussions][disscussions-emacs29]に気軽に書き込んでください。

[releases]: https://github.com/emacs-php/php-mode/releases
[disscussions-emacs29]: https://github.com/emacs-php/php-mode/discussions/751

## インストール

**PHP ModeはEmacs 26.1以降で動作します**。対応バージョンの詳細は[Supported Version]をお読みください。Emacs 28以降では単に以下のコマンドを実行するだけでインストールできます。

```
M-x package-install php-mode
```

[`package-archives`にMELPAを追加][melpa-getting-started]することで、Web上の多くのパッケージでEmacsを強化できます。

パッケージマネージャへの依存なしでインストールしたい場合は、Lispファイルを直接配置する伝統的な方法も可能です。詳しくは[Manual installation][wiki-manual-installation]をお読みください。

## 設定

### 個人設定

.emacsファイル(`~/.emacs.d/init.el`)にPHPモードの設定を記述できます。

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

### プロジェクトローカル設定

プロジェクトのトップディレクトリに`.dir-locals.el`または`.dir-locals-2.el`を記述すると、プロジェクト単位の設定を追加することができます。このファイルはユーザー自身のEmacsにインストールされたパッケージに依存するため、バージョン管理の対象に含めないことを推奨します。

```lisp
((nil
  (php-project-root . git)
  (php-project-coding-style . psr2)))
```

## 不具合を報告する

バグ報告の際には `M-x php-mode-debug` の出力を含めてください。この情報は問題の再現に役立ちます。

貢献するには
-----------------

[CONTRIBUTING.md](CONTRIBUTING.md#japanese)をご覧ください。

## 著作権

PHP Modeは[GNU General Public License Version 3][gpl-v3] (GPLv3) でライセンスされています。

このプロジェクトは1999年に[Turadg Aleahmad][@turadg]が書いた`php-mode.el`に起源を持ちます。2013年に[Daniel Hackney][@haxney]がEmacs組み込みのCC Modeをもとに書き直し始めました。PHPモードの改善に協力した貢献者のリストは[Authors]と[Contributors]に掲載されています。

このプロジェクトは2017年まで[Eric James Michael Ritz][@ejmr]によりメンテナンスされていました。現在は[Friends of Emacs-PHP Development][@emacs-php]コミュニティが引き継いで開発しています。

> ```
> Copyright (C) 2023  Friends of Emacs-PHP development
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
[wiki-manual-installation]: https://github.com/emacs-php/php-mode/wiki/Manual-installation-ja
