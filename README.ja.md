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

> [!IMPORTANT]
> PHP Modeは書き直され、CC Modeに依存しなくなりました。インデントはGNU Emacsに同梱されている`js.el`に由来するエンジンで処理されます。従来のCC Modeベースの実装は`php-cc-mode`として引き続き利用できます。詳しくは[レガシーなCC Mode実装](#レガシーなcc-mode実装-php-cc-mode)をお読みください。

> [!WARNING]
> Emacsをアップグレードした直後に初めてPHPファイルを開いたときに、CC Mode関連のエラーが発生する可能性があります。これは以前のバージョンのEmacsでバイトコンパイルされたPHP Modeがディスクにキャッシュされているために起こるので、PHP Modeの再インストールによって解決します。
>
> **`M-x php-mode-debug-reinstall`** または **`M-x package-reinstall php-mode`** コマンドをお試しください。

[releases]: https://github.com/emacs-php/php-mode/releases
[discussions-emacs30]: https://github.com/emacs-php/php-mode/discussions/798

## インストール

**PHP ModeはEmacs 28.1以降で動作します**([#811])。対応バージョンの詳細は[Supported Version]をお読みください。Emacs 28以降では単に以下のコマンドを実行するだけでインストールできます。

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

### コーディングスタイル

`php-mode`は`php-mode-coding-style`に従ってインデントや編集関連の変数を設定します。既定のスタイルは`per`([PER Coding Style 2.0][per-cs])です。以前のバージョンの既定は`pear`でした。利用できるスタイルは`per`、`psr2`、`pear`、`drupal`、`wordpress`です。

`symfony2`スタイルは削除されました。現代のSymfonyが採用するコーディングスタイルはPERに置き換えられているためです。`php-cc-mode`では引き続き利用できます。

インデントエンジンはもはやCC Modeを使わないため、`php-mode`では`c-basic-offset`はobsoleteです。かわりに`php-indent-offset`をカスタマイズしてください。後方互換のため、プロジェクトが`c-basic-offset`をバッファローカルに設定している場合(`.dir-locals.el`やファイルローカル変数など)は、`php-mode`がその値を`php-indent-offset`に引き継ぎ、警告を表示します。

## 補完

`php-complete.el`は、言語サーバーなしでも使える依存の軽い小さな`completion-at-point`関数(capf)をいくつか提供します。いずれも`M-x`コマンドとしても、[`cape`][cape]の`cape-capf-super`で合成する部品としても使えます。

- `php-complete-complete-function` — 組み込み関数名。
- `php-complete-complete-path` — `__DIR__ . '/...'`イディオムの中でパスを補完します。1階層ずつ辿り、起点は現在のファイルのディレクトリ(実行時に`__DIR__`が解決する先)です。

名前付き関数から登録すると、あとから設定を変更しやすくなります。無名クロージャをフックに追加すると、削除や再定義が困難になります。

```elisp
(defun my-php-mode-setup-completion ()
  "現在のバッファで `php-mode' の補完ソースを有効にする。"
  (add-hook 'completion-at-point-functions
            #'php-complete-complete-path nil t))

(with-eval-after-load 'php-mode
  (add-hook 'php-mode-hook #'my-php-mode-setup-completion))
```

`cape`で複数のオフラインソースを1つの super-capf に合成するには、同じ関数の中で`cape-capf-super`を登録します。

```elisp
(defun my-php-mode-setup-completion ()
  "現在のバッファで `php-mode' の補完ソースを有効にする。"
  (add-hook 'completion-at-point-functions
            (cape-capf-super #'php-complete-complete-function
                             #'php-complete-complete-path)
            nil t))
```

### 文脈依存の `.` キー

`__DIR__`とパス補完を橋渡しする`. '/'`の挿入は、あえてcapfの仕事にはしていません。これは編集環境側に委ねます。そのための primitive が`php-dot-context`です。ポイントが文字列/コメント内か(`string-or-comment`)、文字列リテラルや`__DIR__`のようなマジック定数の直後か(`next-to-string`)、素のコードか(`code`)を返します。capf とこの primitive は「文字列」と「マジック定数」の定義を共有するため、キー挿入と補完の挙動が食い違いません。

たとえば[smartchr][smartchr]では、`.`キーをコード中では`->` / `.` / `. `で循環させ、文字列内ではリテラルの`.`を挿入し、`__DIR__`の直後では`. `を優先(そのまま`php-complete-complete-path`につながる)といった設定が書けます。

```elisp
(defun my-php-smartchr-dot (code within-string next-to-string)
  "`php-dot-context' を使って `.' キーの smartchr を作る。"
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

## レガシーなCC Mode実装 (php-cc-mode)

これまでのPHP ModeはEmacs組み込みのCC Modeの上に実装されていました。この実装は`php-cc-mode`として保存されており、凍結メンテナンス状態です。リグレッション修正のみ受け付け、新規の開発はCC Mode非依存の`php-mode`で行われます。レガシー実装でファイルを開くには`M-x php-cc-mode`を実行するか、自分で`auto-mode-alist`にエントリを追加してください。

既存の設定が動き続けるように、`php-cc-mode`は自身の`php-cc-mode-hook`に加えて`php-mode-hook`も実行します。[CC Modeマニュアル][cc-mode-manual]に記載されている設定が適用されるのは`php-cc-mode`だけです。詳細は`lisp/php-cc-mode.el`冒頭のコメントをお読みください。

## 不具合を報告する

バグ報告の際には `M-x php-mode-debug` の出力を含めてください。この情報は問題の再現に役立ちます。

貢献するには
-----------------

[CONTRIBUTING.md](CONTRIBUTING.md#japanese)をご覧ください。

## 著作権

PHP Modeは[GNU General Public License Version 3][gpl-v3] (GPLv3) でライセンスされています。

このプロジェクトは1999年に[Turadg Aleahmad][@turadg]が書いた`php-mode.el`に起源を持ちます。2013年に[Daniel Hackney][@haxney]がEmacs組み込みのCC Modeをもとに書き直し始めました。2026年にCC Modeへの依存を取り除くための書き直しが行われました。PHPモードの改善に協力した貢献者のリストは[Authors]と[Contributors]に掲載されています。

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
[cape]: https://github.com/minad/cape
[cc-mode-manual]: https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
[gpl-v3]: https://www.gnu.org/licenses/gpl-3.0
[per-cs]: https://www.php-fig.org/per/coding-style/
[smartchr]: https://github.com/imakado/emacs-smartchr
[#811]: https://github.com/emacs-php/php-mode/issues/811
[nongnu-elpa-badge]: https://elpa.nongnu.org/nongnu/php-mode.svg
[nongnu-elpa]: https://elpa.nongnu.org/nongnu/php-mode.html
[melpa-badge]: http://melpa.org/packages/php-mode-badge.svg
[melpa-getting-started]: https://melpa.org/#/getting-started
[melpa-link]: http://melpa.org/#/php-mode
[php-mode]: https://github.com/emacs-php/php-mode
[wiki]: https://github.com/emacs-php/php-mode/wiki
[wiki-manual-installation]: https://github.com/emacs-php/php-mode/wiki/Manual-installation-ja
