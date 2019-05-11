# How to Contribute

This project accepts contributions in various languages.  Not only development but also documentation and translation are important contributions.

 * [English](#english) `[en]`
 * [简体中文](#simplified-chinese) `[zh-Hans]`
 * [繁体中文](#traditional-chinese) `[zh-Hant]`
 * [日本語](#japanese) `[ja]`

----------

## English

### Guideline (en)

All contributions to PHP Mode are welcome.  But please try to do the following when sending improvements or bug fixes:

 1. Add your name to the list of ‘Contributors’ in this `README.md` file if it is not there already.  If you have a GitHub page and/or personal site then please feel free to link your name to it so people can see your other work.
 2. If your contribution addresses an issue on the GitHub project page then include a single line like `GitHub-Issue: #16` with the appropriate issue number.
 3. Make sure to update the constant `php-mode-modified` *only if you patch affects `php-mode.el`,* which means this step is unnecessary for patches related to unit tests.
 4. However, please do not modify `php-mode-version-number`.  The maintainers will decide what constitutes a bump in the version number.
 5. Open the `php-mode-test.el` file and [run all of the tests] to ensure they still pass as expected.  Sometimes we expect for a test to fail, and those unit tests have the appropriate configuration so their failure will not raise any warnings.  You can use `make test` script to run all tests from a terminal, which is also useful in conjunction with [`git bisect run`].
 6. Send us a pull request here on GitHub.
 7. Please make your commit messages as detailed as possible.  It is better to be too verbose than to write too little.  Look at the commits of the maintainers to see many examples of the level of detail that we feel is ideal.  Please never assume that your patch is so simple that future developers will be able to understand the *reason* for the change without comment.  And that is important: your commit message should always strive to answer *"Why"* the patch exists, *"What*" does it accomplish?  The maintainers will sometimes write detailed commit messages for pull-requests by other developers, but please do not rely on us to do this consistently.

If you are fixing a bug related to a GitHub issue, then first of all, thank you for the help improving PHP Mode.  Second, there is a `tests/` directory which contains PHP scripts for issues (although not all of them).  Please consider adding a test script to that directory that documents the expected behavior and provides code that allows others to see if said behavior works properly.  Then create a unit test within `php-mode-test.el` using [ERT]. Please try to follow the format of the existing tests.

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

### Guideline (ja)

**Emacs PHP Mode**はどなたからの貢献も歓迎です。改善やバグ修正を行う前に以下の手順を行ってください。

 1. あなたの名前が`README.md`の“Contributors”のリストになければ追加してください。あなたの名前とGitHubアカウントや個人サイトをリンクして構いません。
 2. もし既にissueとして提起された問題に対処するならば、コミットメッセージに`GitHub-Issue: #16`のような行を含めてください。
 3. `php-mode.el`に影響する変更をした場合、`php-mode-modified`定数を更新してください。テストやドキュメントのみの修正の場合は不要です。
 4. しかし、 `php-mode-version-number` は変更しないでください。メンテナがバージョンを決定します。
 5. `php-mode-test.el`を開いて[すべてのテストを実行][run all of the tests]し、期待通りにテストを通過することを確認します。端末から`make test`で確認することもでき、[`git bisect run`]と併用すると便利です。
 6. GitHubからプルリクエストを送信します
 7. 可能な限り詳細なコミットメッセージを作成してください。不足するよりも冗長すぎる方が良いです。メンテナーのコミットを参照して、私たちが理想的だと期待するコミットメッセージの詳細度として参考にしてください。シンプルすぎるパッチだからと決めつけずにコミットメッセージを書けば、コード中にコメントを書かなくても将来の開発者がコミットの「理由」「経緯」を理解できるようになります。コミットメッセージには「なぜ」コミットを作成したか、「何を」解決するものなのかを記述することが重要です。メンテナーはほかの開発者のプルリクエストに詳細なコミットメッセージを書き込むことがありますが、常に一貫して行われるとは期待しないでください。

**GitHubのissueに関連するバグを修正する場合**： PHPモードの改善に協力いただきありがとうございます！ `tests/`ディレクトリには(すべてではありませんが)issueに関連のあるPHPスクリプトが配置されています。そこに予期される挙動と他のひとが動作を適切に確認できるテストコードを追加することを検討してください。そして`php-mode-test.el`に既存のテストと同じように[ERT]を使ったテストコードを追加してください。

### Regression test for Face (ja)

このプロジェクトでは `foo.php` ファイルに対応した `foo.php.face` ファイルを作成することで[Font Lock]の回帰テストを実現しています。このテストを有効化するには`with-php-mode-test`の引数に`:faces t`オプションを追加してください。

このテストに必要な`.face`ファイルの生成方法は[How to generate face file]を参考にしてください。

[run all of the tests]: http://www.gnu.org/software/emacs/manual/html_node/ert/Running-Tests-Interactively.html#Running-Tests-Interactively
[`git bisect run`]: http://git-scm.com/book/en/Git-Tools-Debugging-with-Git
[ERT]: http://www.gnu.org/software/emacs/manual/html_node/ert/index.html
[Font Lock]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Mode.html
[How to generate face file]: https://github.com/emacs-php/php-mode/issues/509#issuecomment-491528968
