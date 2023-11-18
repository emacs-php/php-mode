# How to Contribute

This project accepts contributions in various languages.  Not only development but also documentation and translation are important contributions.

 * [English](#english) `[en]`
 * [简体中文](#simplified-chinese) `[zh-Hans]`
 * [繁体中文](#traditional-chinese) `[zh-Hant]`
 * [日本語](#japanese) `[ja]`

----------

## English

### Setup (en)

[Eask] is required to build the package. You need to install it using node.js and npm, or [download a prebuilt executable][eask-releases]. See [Eask Introduction][eask-introduction] and [Install Eask][eask-install] for more details.

You can run the tests using the following command:

```sh
make test
```

### Guideline (en)

All contributions to PHP Mode are welcome.  But please try to do the following when submitting enhancements or bug fixes:

 1. Open `php-mode-test.el` and run all the tests to make sure they pass as expected.  You can also check this by running `make test` from the terminal.
 2. Please commit with a concise and clear message that effectively achieves a simple purpose.
 3. Submit a pull request here on GitHub.

If you are fixing a bug related to a GitHub issue, then first of all, thank you for helping to improve PHP Mode.  Second, there is a `tests/` directory that contains PHP scripts for issues (though not all).  Please consider adding a test script to that directory that documents the expected behavior and provides code that allows others to see if that behavior works properly.  Then create a unit test within `php-mode-test.el' using [ERT]. Try to follow the format of the existing tests.

### Regression test for Face (en)

In this project, the regression test of [Font Lock] is realized by creating a `foo.php.face` file to be paired with the` foo.php` file. Add the `: faces t` option to the` with-php-mode-test` argument to activate this test.

Please refer to [How to generate face file] for how to generate `.face` file required for this test.

----------

## Simplified Chinese

I am seeking a contribution on Chinese.  We also lack a README written in Simplified Chinese.

### Guideline (zh-Hans)

Please contribute.

----------

## Traditional Chinese

I am seeking a contribution on Chinese.  We also lack a README written in Traditional Chinese.

### Guideline (zh-Hant)

Please contribute.

----------

## Japanese

### Setup (ja)

パッケージをビルドするにはEaskが必要です。node.jsとnpmからインストールするか、ビルド済み実行ファイルをダウンロードする必要があります。Easkの詳細は[Eask Introduction][eask-introduction]と[Install Eask][eask-install]をお読みください。

以下のコマンドでテストを実行できます。

```sh
make test
```

### Guideline (ja)

**Emacs PHP Mode**はどなたからの貢献も歓迎です。改善やバグ修正を行う前に以下の手順を行ってください。

 1. `php-mode-test.el`を開いて[すべてのテストを実行][run all of the tests]し、期待通りにテストを通過することを確認します。端末から`make test`で確認することもできます。
 2. 簡潔でシンプルな目的を達成するための、明示的なメッセージでコミットしてください。
 3. GitHubからプルリクエストを送信してください。

**GitHubのissueに関連するバグを修正する場合**： PHPモードの改善に協力いただきありがとうございます！ `tests/`ディレクトリには(すべてではありませんが)issueに関連のあるPHPスクリプトが配置されています。そこに予期される挙動と他のひとが動作を適切に確認できるテストコードを追加することを検討してください。そして`php-mode-test.el`に既存のテストと同じように[ERT]を使ったテストコードを追加してください。

### Regression test for Face (ja)

このプロジェクトでは `foo.php` ファイルに対応した `foo.php.face` ファイルを作成することで[Font Lock]の回帰テストを実現しています。このテストを有効化するには`with-php-mode-test`の引数に`:faces t`オプションを追加してください。

このテストに必要な`.face`ファイルの生成方法は[How to generate face file]を参考にしてください。

[run all of the tests]: http://www.gnu.org/software/emacs/manual/html_node/ert/Running-Tests-Interactively.html#Running-Tests-Interactively
[Eask]: https://emacs-eask.github.io/
[eask-introduction]: https://emacs-eask.github.io/Getting-Started/Introduction/
[eask-install]: https://emacs-eask.github.io/Getting-Started/Install-Eask/
[eask-releases]: https://github.com/emacs-eask/cli/releases
[ERT]: http://www.gnu.org/software/emacs/manual/html_node/ert/index.html
[Font Lock]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Mode.html
[How to generate face file]: https://github.com/emacs-php/php-mode/issues/509#issuecomment-491528968
