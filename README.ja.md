<div align="center">
  <h1>Emacs PHP Mode</h1>

[![Emacs: 28.1](https://img.shields.io/badge/Emacs-28.1-blue.svg)](https://www.gnu.org/software/emacs/)
[![lang: PHP 8.1](https://img.shields.io/badge/lang-PHP%208.1-brightgreen.svg)](https://php.net/manual/migration81.php)
[![lang: PHP 7](https://img.shields.io/badge/lang-PHP%207-green.svg)](https://php.net/downloads.php)
[![Build Status](https://github.com/emacs-php/php-mode/workflows/CI/badge.svg)](https://github.com/emacs-php/php-mode/actions)
[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)<br>
[![NonGNU ELPA][nongnu-elpa-badge]][nongnu-elpa]
[![melpa badge][melpa-badge]][melpa-link]

A powerful and flexible Emacs major mode for editing PHP scripts

</div>

[PHP ModeのGitHubプロジェクト][php-mode]にissueを作成してバグ報告や機能リクエストを送ってください。

インストール
------------

**PHP ModeはEmacs 25.2以降で動作します**。古いバージョンのEmacsでも動作するかもしれませんが、保証外です。 古いバージョンのEmacsのPHPモードを使用することによる問題のバグ報告は積極的に対応しません。現在のサポートポリシーは[Supported Version]のページをご覧ください。

### **(推奨)** NonGNU ELPAからのインストール

[![NonGNU ELPA][nongnu-elpa-badge]][nongnu-elpa] [![NonGNU-devel ELPA][nongnu-devel-elpa-badge]][nongnu-devel-elpa]

Emacs 28 (最新安定版) では[NonGNU ELPA](https://elpa.nongnu.org/)がデフォルトのパッケージリポジトリとして追加されています。


### MELPAからのインストール

[![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

GNU Emacs 24以降では、[package][]機能(または[Cask][])を使って[MELPA][]/[MELPA Stable][]からPHP Modeをインストールできます。

### OSのパッケージマネージャからのインストール

PHP Modeは[いくつかのOSが提供するパッケージシステム][php-mode-packages]からインストール可能です。DebianおよびUbuntuのようなDebian派生のOSではEmacsにPHPを追加する最も簡単な方法で、`sudo apt install elpa-php-mode` で導入できます。これらの "Stable" リリースは最新のPHP Modeよりも古いものの、十分にテストされた固定バージョンが提供されます。オートロードとバイトコンパイルは自動で行われます。

最新のPHP Modeの機能とパフォーマンス改善を活用するために**sid** ("unstable"としても知られるローリングリリース)で提供されるバージョンのインストールを検討してください。最新バージョンは[`elpa-php-mode`][elpa-php-mode]にあります。"apt-pinning"を使ってアップデートを自動化することもできます。

また、[Debian 9 (stretch)][php-elisp-stretch]や[Ubuntu 18.10][php-elisp-ubuntu1810]以前で提供されていた `php-elisp` パッケージは[あまりにも古い][issue-430]ので、 **くれぐれもインストールしないでください**。

### 手動でインストール

もしパッケージマネージャに依存したくなければ、伝統的な方法によってLispファイルを直接インストールすることもできます。詳細なセットアップ方法は[手動でのインストール][wiki-manual-installation-ja]ページをご覧ください。

バグを報告する
--------------

報告の際には `M-x php-mode-debug` コマンドを実行して、その出力をバグレポートに含めてください。問題を再現するための手がかりになります。

Settings
--------

### 個人設定

.emacsファイル(`~/.emacs.d/init.el`)にPHPモードでの設定を記述できます。

```lisp
(defun my-php-mode-init ()
  (setq-local show-trailing-whitespace t)
  (setq-local ac-disable-faces '(font-lock-comment-face font-lock-string-face))
  (setq-local page-delimiter "\\_<\\(class\\|function\\|namespace\\)\\_>.+$")

  ;; If you feel phumped and phpcs annoying, invalidate them.
  (when (boundp 'flycheck-disabled-checkers)
    (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
    (add-to-list 'flycheck-disabled-checkers 'php-phpcs)))

(add-hook 'php-mode-hook #'my-php-mode-init)
```

### プロジェクトローカル設定

プロジェクトのトップディレクトリに`.dir-locals.el`を記述すると、プロジェクト単位の設定を追加することができます。このファイルはユーザー自身のEmacsにインストールされたパッケージに依存するため、バージョン管理の対象に含めないことを推奨します。

```lisp
((nil
  (php-project-root . git)
  (php-project-coding-style . psr2)))
```

実験的および作業中の機能
-------------------------------------

### CC Mode, CEDET, EDE, and Semantic

In 2013 Daniel Haxney began rewriting parts of PHP Mode in terms of Emacs' built-in CC Mode.  This laid the foundation for incorporating some of the inherit IDE-ish features of Emacs, such as CEDET, EDE, and Semantic.  Support for these tools continues to improve thanks to the work of Andrea Turso, Steven Rémot, Joris Steyn, and others.  If you wish to test, contribute to, or simply experiment with such features then [this thread is a good place to start](https://github.com/emacs-php/php-mode/issues/256).

### PHP7サポート

PHP7がリリースされました。PHPモードはPHP7からの以下の文法をサポートします。

 1. 返り値の型宣言
 2. `yield from` キーワード
 3. `declare(strict_types=1)` 宣言

機能
--------

### 新しいキーワード

現在のPHPモードはトレイト関連の`insteadof`などのPHP5.4で導入された新しいキーワードを構文強調表示します。また、従来のキーワード`clone`や`default`などもサポートします。

### 定数

強調表示には公式のPHPマニュアルに記載があるすべてのマジック定数と定義済み定数が含まれます。ただし、特定の拡張機能の定数は現在のところ含みません。

### トレイト、インターフェイスと名前空間

トレイト、インターフェイス、名前空間がImenuリストに表示されるようになりました。フォント表示は名前空間でも正しく動作するようになり、`namespace Foo\Bar\Baz`のようなコードはもはや警告されません。`use <namespace> as <alias>`のような名前空間のエイリアスも同様です。現在のところエイリアス名はImenuのリストには含まれませんが、将来のバージョンでは対応予定です。

### アンダースコアの取り扱い

PHPモードは`$foo_bar_baz`のような変数名の単語のそれぞれの部分を移動できるように、アンダースコア(`_`)を「シンボル構成要素 (symbol constituents)」(Emacs用語)として取り扱います。

### メソッドチェーン呼び出し

複数行にわたるメソッド呼び出しを`->`の位置に揃えること(アライメント)ができます。

```php
$object->foo()
       ->bar()
       ->baz();
```

この動作はデフォルトでは無効ですが、カスタマイズ変数 `php-mode-lineup-cascaded-calls` をセットすることで有効化できます。

**注意**: アライメントは、PHPモードのコーディングスタイルのひとつを使用するか、それを継承した場合のみ機能します。

### ネストされた配列の整形

ネストされた関数呼び出しと `array()` 構文は現在デフォルトで(少くとも私の意見では)よく見えるようになりました。例として、このようなスタイルです：

```php
$results = Post::model()->find(
    array(
        'select' => 'title',
        'condition' => 'postID=:postID',
        'params' => array(':postID' => 10),
    )
);
```

### 無名関数

以下のような無名関数

```php
$greet = function($name) { ... };
```

これは現在、Imenuで`$greet`として表示します。

### Flymakeサポート

カスタマイズ変数`php-executable`をセットすることで、コーディング中に警告とエラーをリアルタイムで見るためにFlymakeモードを有効にすることができます。

### ローカルのドキュメントを検索する

コマンド`C-c C-f`でカーソル位置のシンボルをPHP公式サイトのドキュメントから検索できます。また、[ローカルにドキュメントをダウンロード](http://us2.php.net/download-docs.php)してあれば、それを優先します。`php-manual-path`をセットするだけです。もしローカルで発見できなければPHPのWebサイトにフォールバックします。

### 選択範囲内のコードを実行する

`php-send-region`コマンド(デフォルトでは`C-c C-r`)はリージョンで選択された範囲のPHPコードを実行します。`C-x h`と組合せてコード全体を実行することもできます。出力は `*PHP*` バッファに現れます。

### PHPDoc タグ／アノテーション

PHPDocは[JavaDoc](https://en.wikipedia.org/wiki/Javadoc)に似たドキュメンテーションの形式です。

`@param`, `@return`, `@var`... などの表記は**タグ**と呼ばれ、[list of tags defined by phpDocumentor2](https://phpdoc.org/docs/latest/references/phpdoc/tags/index.html)で定義されます。 (これらのタグはPhpStormや[Phan](https://github.com/etsy/phan)といった型チェッカーと互換性があります。)

**アノテーション**と呼ばれる記法は部分的にサポートしています。アノテーションの文法はタグとは少し異なり、`@Annotation(attr1="vvv", attr2="zzz")` のような形式です。

[Symfony](https://symfony.com/)プロジェクトや[Go! AOP](https://github.com/goaop/framework)などいくつかのプロジェクト・フレームワークは[Doctrine Annotations](https://www.doctrine-project.org/projects/doctrine-annotations/en/latest/index.html)の文法を元にしています。

```php
/**
 * Summary of Product class
 *
 * @copyright 2112 John Doe
 * @license https://spdx.org/licenses/Apache-2.0.html Apache License 2.0
 * @ORM\Entity
 * @ORM\Table(name="product")
 */
class Product
{
    /**
     * @ORM\Id
     * @ORM\Column(type="integer")
     * @ORM\GeneratedValue(strategy="AUTO")
     */
    protected $id;

    /**
     * @ORM\Column(type="string", length=100)
     */
    protected $name;

    /**
     * @ORM\Column(type="decimal", scale=2)
     */
    protected $price;

    /**
     * @ORM\Column(type="text")
     */
    protected $description;
}
```

アノテーションは `@` から始まる行で、わかりやすく強調表示されます。ただしPHPモードは[PSR-5: PHPDoc (草案)](https://github.com/phpDocumentor/fig-standards/blob/master/proposed/phpdoc.md)の完全なサポートは実装していません。我々は将来的にこれらの強調表示を厳密にサポートしたいと考えていますが、現在の実装は限定的です。詳しくは[#478](https://github.com/emacs-php/php-mode/issues/478)をご覧ください。

### コーディングスタイル

PHPモードはデフォルトでは`php-enable-default-coding-style`関数で設定される合理的なインデントと整形スタイルを提供します。また、ほかの有用なコーディングスタイルも提供しているので、以下の関数を通じて設定することができます。

1. `php-enable-pear-coding-style`
2. `php-enable-drupal-coding-style`
3. `php-enable-wordpress-coding-style`
4. `php-enable-symfony2-coding-style`
5. `php-enable-psr2-coding-style`

`M-x customize-group <RET> php`で‘PHP Mode Coding Style’を探してカスタマイズメニューを探して、デフォルトのコーディングスタイルの設定を有効化することができます。以下のように、コーディングスタイルごとにフックを有効化することもできます。

```lisp
(add-hook 'php-mode-hook 'php-enable-default-coding-style)
```

#### Symfony2 Style

このスタイルではメソッドチェーンのインデントの継続とぶらさがったセミコロンを整形できます。


```php
    $user1
        ->setCreateDate(new \DateTime('2007-05-07 01:34:45'))
        ->setLastDate(new \DateTime('2012-08-18 19:03:02'))
        ->setUsername('jay')
    ;
```

このスタイルはSymfony2のコードベースで広く利用されていますが、慣習についての文書で明示的に言及されているものではありません。

### HTMLテンプレートのサポートを無効化する

多くの開発者はPHPモードで純粋なPHPスクリプト(HTMLテンプレートを含まないもの)を編集します。HTMLとの互換レイヤーはPHPモードの歴史的な機能ですが、完全には機能していません。速度の低下や強調表示を破壊するおそれがあるなどの副作用があります。変数`php-template-compatibility`を`nil`にセットすると、HTMLとの互換性を無効化することができます。HTMLやその他のマークアップ言語のテンプレートエンジンを含むPHPスクリプトを開発する際は[Web Mode][]は優れた選択肢です。

### Subword Mode

GNU Emacsには[Subword Mode][]という機能があり、このマイナーモードは[キャメルケース][camelCase]の部分を別の単語のように移動することができます。たとえば、PHPモードはデフォルトでは変数`$fooBarBaz`を一つの単語として扱います。しかしSubword Modeを有効にすればEmacsはこの変数名を3つの単語として扱い、単語関係のコマンド(`M-f`, `M-b`, `M-d`など)はカーソル位置のキャメルケースの各部分に影響します。

もしPHPファイルで常にSubword Modeを有効化したいならば、以下のように設定できます。

```lisp
(add-hook 'php-mode-hook (lambda () (subword-mode 1)))
```

キーバインド `C-c C-w` はSubword Modeのオンとオフを切り替えます。

### 現在のclass/namespaceを挿入する

```el
(with-eval-after-load 'php-mode
  (define-key php-mode-map (kbd "C-c C--") 'php-current-class)
  (define-key php-mode-map (kbd "C-c C-=") 'php-current-namespace))
```

PHPプログラミングのためのパッケージ
----------------------------------

- 入力補完
  - [ac-php](https://github.com/xcwen/ac-php): [company-mode](https://github.com/company-mode/company-mode) and [auto-complete](https://github.com/auto-complete/auto-complete) for PHP
- 構文チェック
  - [flycheck](https://github.com/flycheck/flycheck/): On the fly syntax checker
  - [flymake-php](https://github.com/purcell/flymake-php): flymake for PHP files
- スニペット
  - [php-auto-yasnippets](https://github.com/ejmr/php-auto-yasnippets): Dynamically Generated YASnippets for PHP Code
- ドキュメント
  - [ggtags](https://github.com/leoliu/ggtags): eldoc by using GNU global tags
  - [php-eldoc](https://github.com/sabof/php-eldoc): eldoc backend for PHP
- テスト
  - [phpunit](https://github.com/nlamirault/phpunit.el): phpunit test command tool
- コーディングスタイル
  - [phpcbf](https://github.com/nishimaki10/emacs-phpcbf): PHP_CodeSniffer for Emacs
- Semantic
  - [ede-php-autoload](https://github.com/stevenremot/ede-php-autoload): Semantic for PHP
- フレームワーク
  - [cake](https://github.com/k1LoW/emacs-cake): minor-mode for CakePHP
  - [cake2](https://github.com/k1LoW/emacs-cake2): minor-mode for CakePHP2


貢献するには
-----------------

[CONTRIBUTING.md](CONTRIBUTING.md#japanese)をご覧ください。

Wiki
--------

GitHubのプロジェクトページには[wiki][]があり、自由に編集して構いません。このWikiには今後追加する計画のある機能やバグが掲載されています。また、PHPモードをより使いやすくするためのTipsを追加できます。

## 著作権

PHP Modeは[GNU General Public License Version 3][gpl-v3] (GPLv3) でライセンスされています。

このプロジェクトは1999年に[Turadg Aleahmad][@turadg]が書いた`php-mode.el`に起源を持ちます。2013年に[Daniel Hackney][@haxney]がEmacs組み込みのCC Modeをもとに書き直し始めました。PHPモードの改善に協力した貢献者のリストは[Authors]と[Contributors]に掲載されています。

このプロジェクトは2017年まで[Eric James Michael Ritz][@ejmr]によりメンテナンスされていました。現在は[Friends of Emacs-PHP Development][@emacs-php]コミュニティが引き継いで開発しています。

> ```
> Copyright (C) 2018-2020  Friends of Emacs-PHP development
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
[Cask]: https://github.com/cask/cask
[Contributors]: https://github.com/emacs-php/php-mode/graphs/contributors
[MELPA Stable]: https://stable.melpa.org/
[MELPA]: https://melpa.org/
[Subword Mode]: https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
[Supported Version]: https://github.com/emacs-php/php-mode/wiki/Supported-Version
[Web Mode]: http://web-mode.org/
[camelCase]: https://ja.wikipedia.org/wiki/%E3%82%AD%E3%83%A3%E3%83%A1%E3%83%AB%E3%82%B1%E3%83%BC%E3%82%B9
[cc mode]: https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
[elpa-php-mode]: https://packages.debian.org/sid/elpa-php-mode
[gpl-v3]: https://www.gnu.org/licenses/quick-guide-gplv3.html
[issue-430]: https://github.com/emacs-php/php-mode/issues/430
[nongnu-devel-elpa-badge]: https://elpa.nongnu.org/nongnu-devel/php-mode.svg
[nongnu-devel-elpa]: https://elpa.nongnu.org/nongnu-devel/php-mode.html
[nongnu-elpa-badge]: https://elpa.nongnu.org/nongnu/php-mode.svg
[nongnu-elpa]: https://elpa.nongnu.org/nongnu/php-mode.html
[melpa-badge]: http://melpa.org/packages/php-mode-badge.svg
[melpa-link]: http://melpa.org/#/php-mode
[melpa-stable-badge]: http://stable.melpa.org/packages/php-mode-badge.svg
[melpa-stable-link]: http://stable.melpa.org/#/php-mode
[package]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html
[php-elisp-stretch]: https://packages.debian.org/stretch/php-elisp
[php-elisp-ubuntu1810]: https://packages.ubuntu.com/cosmic/php-elisp
[php-mode-packages]: https://repology.org/project/emacs:php-mode/versions
[php-mode]: https://github.com/emacs-php/php-mode
[php-suite]: https://github.com/emacs-php/php-suite
[wiki]: https://github.com/emacs-php/php-mode/wiki
[wiki-manual-installation-ja]: https://github.com/emacs-php/php-mode/wiki/Manual-installation-ja
