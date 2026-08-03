<div align="center">
  <h1>Emacs PHP Mode</h1>

[![Emacs: 30.0](https://img.shields.io/badge/Emacs-30.0-blue.svg)](https://www.gnu.org/software/emacs/)
[![lang: PHP 8.4](https://img.shields.io/badge/lang-PHP%208.4-brightgreen.svg)](https://www.php.net/releases/8.4/)
[![Build Status](https://github.com/emacs-php/php-mode/workflows/CI/badge.svg)](https://github.com/emacs-php/php-mode/actions)
[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)][gpl-v3]<br>
[![NonGNU ELPA][nongnu-elpa-badge]][nongnu-elpa]
[![melpa badge][melpa-badge]][melpa-link]

A powerful and flexible Emacs major mode for editing PHP scripts

[English](README.md) &nbsp;&nbsp;|&nbsp;&nbsp; 日本語

</div>

[GitHubプロジェクト][php-mode]にissueを作成してバグ報告や機能リクエストを送ってください。

> [!NOTE]
> [最新版][releases]のPHP ModeはEmacs 30をサポートしています。<br />アップグレードに伴うトラブルは[Discussions][discussions-emacs30]に気軽に書き込んでください。

> [!WARNING]
> Emacsをアップグレードした直後に初めてPHPファイルを開いたときに、CC Mode関連のエラーが発生する可能性があります。これは以前のバージョンのEmacsでバイトコンパイルされたPHP Modeがディスクにキャッシュされているために起こるので、PHP Modeの再インストールによって解決します。
>
> **`M-x php-mode-debug-reinstall`** または **`M-x package-reinstall php-mode`** コマンドをお試しください。

[releases]: https://github.com/emacs-php/php-mode/releases
[discussions-emacs30]: https://github.com/emacs-php/php-mode/discussions/798

## インストール

**PHP ModeはEmacs 27.1以降で動作します**。対応バージョンの詳細は[Supported Version]をお読みください。Emacs 28以降では単に以下のコマンドを実行するだけでインストールできます。

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

### PHP-IDE: LSPクライアントおよびPhpactorとの連携

`php-ide`(`php-ide.el`)は、PHP Modeと[Eglot](https://github.com/joaotavora/eglot)・[lsp-mode](https://github.com/emacs-lsp/lsp-mode)・[lsp-bridge](https://github.com/manateelazycat/lsp-bridge)・[Phpactor](https://github.com/emacs-php/phpactor.el)のようなIDE的機能を橋渡しする**実験的**な機能です。これらの機能自体を実装するものではなく、選んだものを`php-ide-mode`という単一のマイナーモード経由で有効化・無効化するだけです。詳細な仕様は`php-ide.el`冒頭のCommentaryを参照してください。

```lisp
(defun my-php-mode-init ()
  (add-hook 'hack-local-variables-hook 'php-ide-turn-on nil t))

(with-eval-after-load 'php-ide
  (custom-set-variables
   '(php-ide-features '(eglot))              ;; '(phpactor)、'(lsp-mode)、'(lsp-bridge)も可
   '(php-ide-eglot-executable 'intelephense)  ;; 'phpactor、パス文字列、文字列のリストも可
   '(php-ide-mode-lighter "")))               ;; モードラインからPHP-IDEを隠す
```

`php-ide-turn-on`は`php-ide-features`が未設定のときは何もしないため、上記のように無条件にフックへ追加しても安全です。上記のようにグローバルに、または下記のようにプロジェクト単位で`php-ide-features`を設定するまでPHP-IDEはオフのままです。

`php-ide`読み込み後に使えるコマンド:

* `M-x php-ide-mode` — `php-ide-features`に基づき、現在のバッファでPHP-IDEをトグルする。
* `M-x php-ide-turn-on` — 同様だが、`php-ide-features`が未設定でもエラーにならない。
* `M-x php-ide-set-feature` — このシステムで実際に利用可能な(対応パッケージがインストールされている)機能から対話的に選び、現在のバッファで有効化する。
* `M-x php-ide-status` — PHP-IDEが有効かどうか、設定内容、利用可能な機能を表示する。

#### プロジェクト単位のPHP-IDE設定

```lisp
((nil (php-project-root . git)
      (php-ide-features . (eglot))))
```

`php-ide-features`と`php-ide-eglot-executable`は、PHP-IDE組み込みの機能名やバンドル済み実行ファイルのプリセット(`intelephense`、`phpactor`など)を指す場合に限り、`.dir-locals.el`での設定が安全とみなされます。それ以外の値——生の実行ファイルパス、明示的なコマンド引数、カスタムの`php-ide-mode-functions`フックなど——は、これまで通りEmacsの「危険な変数」に対する通常の確認を経ます。そうしないと、ファイルを開くだけで任意のリポジトリが任意のコマンド(またはLisp関数)をあなたのEmacs上で実行できてしまうためです。

### `project.el`・Projectileとの連携

`php-project-get-root-dir`は、まずPHP固有のマーカー（`.projectile`、`composer.json`/`composer.lock`、続いてVCSディレクトリ）を探索します。モノレポではパッケージ単位の`vendor/autoload.php`やコーディングスタイルが`php-mode`にとって重要なため、VCSルートより`composer.json`を優先します。これらのマーカーが見つからない場合は`project-current`にフォールバックするので、任意の[`project.el`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html)バックエンドが検出に寄与できます。

* **Projectile 3**は`projectile-mode`有効時に自身を`project-find-functions`へ登録するため、その検出結果が自動的に利用されます。従来の`php-project-use-projectile-to-detect-root`オプションはこのため廃止予定です。
* Projectileなしでも`project-vc-extra-root-markers`（Emacs 29以降）で追加のルートマーカーを宣言できます。例えば`.dir-locals.el`に次のように記述します。

  ```lisp
  ((nil
    (project-vc-extra-root-markers . ("composer.json"))))
  ```

逆に、PHP固有の検出（上記の`composer.json`優先ルール）をEglotや`project-find-file`などの`project.el`利用側に見せたい場合は、組み込みのVC検出を補完するだけになるよう、低い優先度で`php-project-project-find-function`を登録します。

```lisp
(add-hook 'project-find-functions #'php-project-project-find-function 90)
```

## HTMLとPHPが混在するファイルの編集

`php-mode`は純粋なPHPスクリプトのためのメジャーモードです。テンプレートのようにHTMLの中にPHPを埋め込んだファイルは、両方の言語を理解するメジャーモードで編集するほうが適しています。特にインデントは、HTML部分を素の`php-mode`で編集すると正しく動作しません。

そうしたファイルのために、PHP Modeは`php-html-template-major-mode`(既定は[`web-mode`](https://web-mode.org/))へ処理を委ねます。好みのモードを設定できます。

```lisp
(setopt php-html-template-major-mode 'web-mode)
```

### メジャーモードの選ばれ方

拡張子`.php`のファイルは`php-mode-maybe`を通して開かれ、ファイル名と内容からメジャーモードが決まります。

- `php-template-mode-alist`にマッチするファイル名(例: `.phtml`や`.blade.php`)は、対応するテンプレート用モードで開きます。
- それ以外は、ディレクトリローカル変数`php-project-php-file-as-template`に従います。
  - `auto`(既定): HTMLタグを含むファイルを`php-html-template-major-mode`に切り替えます。
  - `t`: そのディレクトリのすべての`.php`ファイルをテンプレートとして扱います。
  - `nil`: すべての`.php`ファイルを素のPHPスクリプトとして扱います。
- いずれにも当てはまらない場合は`php-default-major-mode`(`php-mode`)で開きます。

`php-project-php-file-as-template`は`.dir-locals.el`でプロジェクトごとに設定できます。

```lisp
((nil
  (php-project-php-file-as-template . nil)))
```

### php-modeからの切り替え

すでに`php-mode`でHTMLタグを含むファイルをインデントしようとすると、PHP Modeは警告し、`php-html-template-major-mode`への切り替えを尋ねます。このプロンプトを無効にするには`php-mode-warn-if-html-template`を`nil`に設定してください。

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
