PHP Mode for GNU Emacs
======================

[![lang: PHP 7](https://img.shields.io/badge/lang-PHP%207-brightgreen.svg)](http://php.net/manual/migration70.php)
[![lang: PHP 5](https://img.shields.io/badge/lang-PHP%205-green.svg)](http://php.net/downloads.php)
[![travis badge][travis-badge]][travis-link]
[![melpa badge][melpa-badge]][melpa-link]
[![melpa stable badge][melpa-stable-badge]][melpa-stable-link]
[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)

PHP 5.4以降を開発しやすくするための機能をアップデートするプロジェクトです。これは以下の人々の作業をもとにフォークされました。

1. Turadg Aleahmad (Original Author)
2. Aaron S. Hawley
3. Lennart Borgman
4. Eric James Michael Ritz
5. Syohei Yoshida

リストアップされたすべての貢献者たちも同様にPHPモードを改善しました。

現在のメンテナ：

1. USAMI Kenta (@zonuexe)

[PHP ModeのGitHubプロジェクト][php-mode]にissueを作成してバグ報告や機能リクエストを送ってください。あるいは[PHP suite][php-suite]の[FeatHubページ][feathub]に機能リクエストを送っても構いません。

インストール
------------

**PHPモードはEmacs 24.3以降で動作します**。古いバージョンのEmacsでも動作するかもしれませんが、保証外です。 古いバージョンのEmacsのPHPモードを使用することによる問題のバグ報告は積極的に対応しません。

GNU Emacs 24以降では、[package][]機能を使って[MELPA][]からPHPモードをインストールすることができます。 *[Marmalade][]パッケージリポジトリには2004年のオリジナル版PHPモードしか登録されていません*。そのため、MELPAを使用してPHPモードをインストールすることを推奨します。単にパッケージマネージャを使いたくない場合は、`php-mode.el`ファイルをダウンロードして`load-path`の通ったディレクトリに配置し、必要に応じて `(require 'php-mode)` をEmacsの設定に追加すると、PHPファイルを開くたびに自動的にPHPモードが有効になります。

さらに`skeleton/php-ext.el`を`load-path`に追加することで[テンプレートを有効にする](https://www.gnu.org/software/emacs/manual/html_node/autotype/index.html#Top)ことができます。

```lisp
(eval-after-load 'php-mode
  '(require 'php-ext))
```

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

この動作はデフォルトでは無効ですが、カスタマイズ変数 `php-lineup-cascaded-calls` セットすることで有効化できます。

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

[Symfony](http://symfony.com/)プロジェクトや[Go! AOP](https://github.com/goaop/framework)などいくつかのプロジェクト・フレームワークは[Doctrine Annotations](http://docs.doctrine-project.org/projects/doctrine-common/en/latest/reference/annotations.html)の文法を元にしています。

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

### 定数の追加

フレームワークによって一般的な定数群が定義されるならば、変数`php-extra-constants`を値をカスタマイズすることができます。これはPHPモードが追加の定数として扱う定数名の文字列のリストです。これらの定数はPHP組み込みの定数と同じ強調表示を行います。

### Web Mode定数とキーワード

もしあなたが[Web Mode]を使用しているなら、Web Modeに設定された追加のPHP定数をとキーワードを追加しようとします。

### HTMLテンプレートのサポートを無効化する

多くの開発者はPHPモードで純粋なPHPスクリプト(HTMLテンプレートを含まないもの)を編集します。HTMLとの互換レイヤーはPHPモードの歴史的な機能ですが、完全には機能していません。速度の低下や強調表示を破壊するおそれがあるなどの副作用があります。変数`php-template-compatibility`を`nil`にセットすると、HTMLとの互換性を無効化することができます。HTMLやその他のマークアップ言語のテンプレートエンジンを含むPHPスクリプトを開発する際は[Web Mode][]は優れた選択肢です。

### Subword Mode

GNU Emacsには[Subword Mode][]という機能があり、このマイナーモードは[キャメルケース][camelCase]の部分を別の単語のように移動することができます。たとえば、PHPモードはデフォルトでは変数`$fooBarBaz`を一つの単語として扱います。しかしSubword Modeを有効にすればEmacsはこの変数名を3つの単語として扱い、単語関係のコマンド(`M-f`, `M-b`, `M-d`など)はカーソル位置のキャメルケースの各部分に影響します。

もしPHPファイルで常にSubword Modeを有効化したいならば、以下のように設定できます。

```lisp
(add-hook 'php-mode-hook (lambda () (subword-mode 1)))
```

キーバインド `C-c C-w` はSubword Modeのオンとオフを切り替えます。

### Amakaサポート

Viewing and editing build scripts for [Amaka](http://trashofmasters.github.io/amaka/) will automatically enable PHP Mode.

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

PHPモードはどなたからの貢献も歓迎です。改善やバグ修正を行う前に以下の手順を行ってください。

 1. あなたの名前が`README.md`の“Contributors”のリストになければ追加してください。あなたの名前とGitHubアカウントや個人サイトをリンクして構いません。
 2. もし既にissueとして提起された問題に対処するならば、コミットメッセージに`GitHub-Issue: #16`のような行を含めてください。
 3. `php-mode.el`に影響する変更をした場合、`php-mode-modified`定数を更新してください。テストやドキュメントのみの修正の場合は不要です。
 4. しかし、 `php-mode-version-number` は変更しないでください。メンテナがバージョンを決定します。
 5. `php-mode-test.el`を開いて[すべてのテストを実行](http://www.gnu.org/software/emacs/manual/html_node/ert/Running-Tests-Interactively.html#Running-Tests-Interactively)し、期待通りにテストを通過することを確認します。端末から`make test`で確認することもでき、[`git bisect run`](http://git-scm.com/book/en/Git-Tools-Debugging-with-Git)と併用すると便利です。
 6. GitHubからプルリクエストを送信します
 7. 可能な限り詳細なコミットメッセージを作成してください。不足するよりも冗長すぎる方が良いです。メンテナーのコミットを参照して、私たちが理想的だと期待するコミットメッセージの詳細度として参考にしてください。シンプルすぎるパッチだからと決めつけずにコミットメッセージを書けば、コード中にコメントを書かなくても将来の開発者がコミットの「理由」「経緯」を理解できるようになります。コミットメッセージには「なぜ」コミットを作成したか、「何を」解決するものなのかを記述することが重要です。メンテナーはほかの開発者のプルリクエストに詳細なコミットメッセージを書き込むことがありますが、常に一貫して行われるとは期待しないでください。

**GitHubのissueに関連するバグを修正する場合**： PHPモードの改善に協力いただきありがとうございます！ `tests/`ディレクトリには(すべてではありませんが)issueに関連のあるPHPスクリプトが配置されています。そこに予期される挙動と他のひとが動作を適切に確認できるテストコードを追加することを検討してください。そして`php-mode-test.el`に既存のテストと同じように[ERT][]を使ったテストコードを追加してください。

Wiki
--------

GitHubのプロジェクトページには[wiki][]があり、自由に編集して構いません。このWikiには今後追加する計画のある機能やバグが掲載されています。また、PHPモードをより使いやすくするためのTipsを追加できます。

メーリングリスト
----------------

The “emacs-php” mailing list is a place to discuss PHP Mode as well as all other PHP-related packages for Emacs.  You can find the mailing list at:

1. [emacs-php at Google Groups](https://groups.google.com/forum/#!forum/emacs-php)
2. [Gmane](http://dir.gmane.org/gmane.emacs.php)

We encourage all users of PHP Mode *and* developers of any PHP-related packages to feel free to post anything there regarding PHP and Emacs.


ライセンス
-------

PHP Mode uses the [GNU General Public License 3](http://www.gnu.org/copyleft/gpl.html).


Contributors
------------

PHPモードの改善に協力したすべての貢献者のリストは[README.md#contributors](https://github.com/emacs-php/php-mode/blob/master/README.md#contributors)に掲載されています。

[wiki]: https://github.com/emacs-php/php-mode/wiki
[ert]: http://www.gnu.org/software/emacs/manual/html_node/ert/index.html
[cc mode]: https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
[Subword Mode]: https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
[camelCase]: https://ja.wikipedia.org/wiki/%E3%82%AD%E3%83%A3%E3%83%A1%E3%83%AB%E3%82%B1%E3%83%BC%E3%82%B9
[package]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html
[MELPA]: http://melpa.milkbox.net/
[Marmalade]: http://marmalade-repo.org/
[Web Mode]: http://web-mode.org/
[travis-badge]: https://travis-ci.org/emacs-php/php-mode.svg
[travis-link]: https://travis-ci.org/emacs-php/php-mode
[melpa-link]: http://melpa.org/#/php-mode
[melpa-stable-link]: http://stable.melpa.org/#/php-mode
[melpa-badge]: http://melpa.org/packages/php-mode-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/php-mode-badge.svg
[php-mode]: https://github.com/emacs-php/php-mode
[php-suite]: https://github.com/emacs-php/php-suite
[feathub]: https://feathub.com/emacs-php/php-suite
