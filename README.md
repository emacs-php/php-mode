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

Please submit any bug reports or feature requests by creating issues on [the GitHub page for PHP Mode][php-mode].

Installation
------------

**PHP Mode works on Emacs 25.2 or later.**  PHP Mode may work with older versions of Emacs but this is not guaranteed.  Bug reports for problems related to using PHP Mode with older versions of Emacs will most like *not* be addressed.

The current support policy can be found on the [Supported Version] page.

### **(RECOMMENDED)** Install from NonGNU ELPA

[![NonGNU ELPA][nongnu-elpa-badge]][nongnu-elpa] [![NonGNU-devel ELPA][nongnu-devel-elpa-badge]][nongnu-devel-elpa]

Emacs 28 (latest stable release) includes [NonGNU ELPA](https://elpa.nongnu.org/) as the default package repository.

### Install from MELPA

[![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

With GNU Emacs 24 or later then you can use its [package][] feature (or [Cask][]) to install PHP Mode from [MELPA][] or [MELPA Stable][].

### Install using OS package system

PHP Mode is available from [package systems provided by several OSs][php-mode-packages].  On Debian, and operating systems derived from it, such as Ubuntu, the easiest way to add PHP support to Emacs is `sudo apt install elpa-php-mode`.  "Stable" releases of these operating systems include a well-tested frozen version of this package that is always older than the latest upstream version of PHP Mode.  Autoloads and byte-compilation are automatic.

Please consider installing the package provided in `sid` (aka: "unstable", a rolling release) in order to benefit from the latest PHP Mode features and performance improvements.  The latest version can be found here: [`elpa-php-mode`][elpa-php-mode].  Investigate "apt-pinning" to make tracking this update stream automatic.

Also, the `php-elisp` package provided by [Debian 9 (stretch)][php-elisp-stretch] and [Ubuntu 18.10][php-elisp-ubuntu1810] and earlier is [extremely old][issue-430], so **PLEASE DO NOT INSTALL IT**.

### Manual installation

If you don't want to depend on a package manager, you can install Lisp files directly in the traditional way.  See [Manual installation][wiki-manual-installation] for our recommended setup method.

### Configuration

PHP Mode's default style might not be what you expect.  Before filing a bug, please try the following:  `C-h v php-mode-coding-style`, to get the list of available styles.  Then use `M-x php-set-style` for each of these, to check to see if one of them is to your liking.  Use `TODO: Please comment on what config method you'd like to recommend to users`

Reporting Bugs
--------------

When reporting a bug please run the function `M-x php-mode-debug` and include its output in your bug report.  This helps up reproduce any problem you may have.


Experimental and In-Progress Features
-------------------------------------

### CC Mode, CEDET, EDE, and Semantic

In 2013 Daniel Haxney began rewriting parts of PHP Mode in terms of Emacs' built-in CC Mode.  This laid the foundation for incorporating some of the inherit IDE-ish features of Emacs, such as CEDET, EDE, and Semantic.  Support for these tools continues to improve thanks to the work of Andrea Turso, Steven Rémot, Joris Steyn, and others.  If you wish to test, contribute to, or simply experiment with such features then [this thread is a good place to start](https://github.com/emacs-php/php-mode/issues/256).

### PHP 7 Support

PHP 7 has been released.  PHP Mode supports the following features and changes from PHP 7:

1. Type-hints for return values in functions and methods receive syntax highlighting in the same way as type-hints for function and method parameters.

2. PHP Mode treats `yield from` as keyword in the same way it already does for a sole `yield`.

3. It recognizes `strict_types` as a special declaration in the same way as `ticks`.


Features
--------

### New Keywords

Now PHP Mode supports syntax highlighting for new keywords which PHP 5.4 introduced, e.g. those related to traits, such as `insteadof`.  Also supported are the older keywords `clone` and `default`.

### Constants

Syntax highlighting includes every magic constant and predefined constant listed on the official PHP site.  However, some constants from specific extensions are not currently included.

### Traits, Interfaces, and Namespaces

Traits, interfaces, and namespaces now appear under Imenu listings. Fontification behaves properly for namespaces as well, so that code like `namespace Foo\Bar\Baz` no longer looks like a warning.  This is also true for namespace aliases, e.g. `use <namespace> as <alias>`; currently the aliased name is not listed in Imenu, but future versions will address this.

### Treatment of Underscores

PHP Mode treats underscores as ‘symbol constituents’ (in Emacs terminology) so that you can use keys like `M-f` and `M-b` to move through the individual parts of a variable name like `$foo_bar_baz`.

### Chained Method Calls

PHP Mode can align method calls over multiple lines anchored around the `->` operator, e.g.:

```php
$object->foo()
       ->bar()
       ->baz();
```

This behaviour is off by default, but you can customize the variable `php-mode-lineup-cascaded-calls` to enable this.

**Note:** Alignment will only work if you use one of the php-mode coding styles or inherit one of the styles.

### Nested Array Formatting

Nested function calls and `array()` structures now look better by default (or at least in my opinion).  Here is an example of the style:

```php
$results = Post::model()->find(
    array(
        'select' => 'title',
        'condition' => 'postID=:postID',
        'params' => array(':postID' => 10),
    )
);
```

### Anonymous Functions

Anonymous functions such as

```php
$greet = function($name) { ... };
```

will now appear on Imenu; in this case the name will be `$greet`.

### Flymake Support

By customizing the variable `php-executable` you can enable Flymake mode in order to see warnings and errors in real-time as you write code.

### Search Local Documentation

The key command `C-c C-f` will search the PHP website for documentation on the word under the cursor.  However, if you have a [local copy of the PHP documentation](http://us2.php.net/download-docs.php) then PHP Mode will try searching that documentation first.  All you need to do is customize the variable `php-manual-path` and give it the path to your copy of the documentation.  If PHP Mode cannot find something locally then it will still fallback on searching the PHP website.

### Executing Regions of PHP

The command `php-send-region`, which is bound to `C-c C-r` by default, will execute the selected region of PHP code.  In conjunction with the Emacs command `C-x h` you can use this to execute an entire file.  Any output will appear in a buffer called `*PHP*`.

### PHPDoc Tag / Annotation Highlighting

PHPDoc is a documentation format similar to [JavaDoc](https://en.wikipedia.org/wiki/Javadoc).

There are `@param`, `@return`, `@var`... etc in the notation called **tag**, look at [list of tags defined by phpDocumentor2](https://phpdoc.org/docs/latest/references/phpdoc/tags/index.html).  (These tags are compatible with static type checkers like PhpStorm and [Phan](https://github.com/etsy/phan).)

In addition, it also partially supports notation called **annotation**.  Annotation has a slightly different grammar from tag, and the example is `@Annotation(attr1="vvv", attr2="zzz")`.

[Symfony](https://symfony.com/) project and [Go! AOP](https://github.com/goaop/framework) and some projects/frameworks use annotation grammer based on [Doctrine Annotations](https://www.doctrine-project.org/projects/doctrine-annotations/en/latest/index.html).

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

The annotations are the lines that begin with the `@` character, and PHP Mode will give these special highlighting to help them stand out.

PHP Mode has not fully supported [PSR-5: PHPDoc (Draft)](https://github.com/phpDocumentor/fig-standards/blob/master/proposed/phpdoc.md) yet.  We want to support them, but the current implementation still limited.  See issue [#478](https://github.com/emacs-php/php-mode/issues/478) for details.

### Coding Styles

By default PHP Mode tries to provide a reasonable style for indentation and formatting, which you can use via the function `php-enable-default-coding-style`.  However, it provides other options suited for particular projects which you may find useful.  Other coding styles are available through these functions:

1. `php-enable-pear-coding-style`
2. `php-enable-drupal-coding-style`
3. `php-enable-wordpress-coding-style`
4. `php-enable-symfony2-coding-style`
5. `php-enable-psr2-coding-style`

They will help format your code for PEAR/PSR-2 projects, or work on Drupal, WordPress, and Symfony2 software, respectively.  You may enable any of them by default by running `M-x customize-group <RET> php` and looking for the ‘PHP Mode Coding Style’ option.  You may also enable any of these via a hook, e.g.

```lisp
(add-hook 'php-mode-hook 'php-enable-default-coding-style)
```

#### Symfony2 Style

With this style method call chains can be formatted with indented continuation and a hanging semi-colon:

```php
    $user1
        ->setCreateDate(new \DateTime('2007-05-07 01:34:45'))
        ->setLastDate(new \DateTime('2012-08-18 19:03:02'))
        ->setUsername('jay')
    ;
```

This style is used widely throughout Symfony2 source code even if it is not explicitly mentioned in their conventions documents.

### Avoid HTML Template Compatibility

Many developers use PHP Mode to edit pure PHP scripts (e.g. files with only PHP and no HTML). A basic compatibility layer with HTML has historically been part of PHP Mode but it does not work perfectly and can cause some bad side effects such as slowness and incorrect font locking.  Configuring the `php-template-compatibility` property with a `nil` will cancel any attempt of HTML compatibility.  [Web Mode](http://web-mode.org/) is a great alternative to PHP Mode if you need to work with PHP scripts that do contain HTML and other markup.

### Subword Mode

GNU Emacs comes with [Subword Mode][], a minor mode that allows you to navigate the parts of a [camelCase][] as if they were separate words.  For example, PHP Mode treats the variable `$fooBarBaz` as a whole name by default.  But if you enable Subword Mode then Emacs will treat the variable name as three separate words, and therefore word-related commands (e.g. `M-f`, `M-b`, `M-d`) will only affect the camelCase part of the name under the cursor.

If you want to always use Subword Mode for PHP files then you can add this to your Emacs configuration:

```lisp
(add-hook 'php-mode-hook (lambda () (subword-mode 1)))
```

The key-binding `C-c C-w` will also toggle Subword Mode on and off.

### Insert current class/namespace

```el
(with-eval-after-load 'php-mode
  (define-key php-mode-map (kbd "C-c C--") 'php-current-class)
  (define-key php-mode-map (kbd "C-c C-=") 'php-current-namespace))
```

Other Packages for PHP programming
----------------------------------

- Completions
  - [ac-php](https://github.com/xcwen/ac-php): [company-mode](https://github.com/company-mode/company-mode) and [auto-complete](https://github.com/auto-complete/auto-complete) for PHP
- Syntax checking
  - [flycheck](https://github.com/flycheck/flycheck/): On the fly syntax checker
  - [flymake-php](https://github.com/purcell/flymake-php): flymake for PHP files
- Snippet
  - [php-auto-yasnippets](https://github.com/ejmr/php-auto-yasnippets): Dynamically Generated YASnippets for PHP Code
- Documentation
  - [ggtags](https://github.com/leoliu/ggtags): eldoc by using GNU global tags
  - [php-eldoc](https://github.com/sabof/php-eldoc): eldoc backend for PHP
- Testing
  - [phpunit](https://github.com/nlamirault/phpunit.el): phpunit test command tool
- Style
  - [phpcbf](https://github.com/nishimaki10/emacs-phpcbf): PHP_CodeSniffer for Emacs
- Semantic
  - [ede-php-autoload](https://github.com/stevenremot/ede-php-autoload): Semantic for PHP
- Framework
  - [cake](https://github.com/k1LoW/emacs-cake): minor-mode for CakePHP
  - [cake2](https://github.com/k1LoW/emacs-cake2): minor-mode for CakePHP2


How to Contribute
-----------------

Please see [CONTRIBUTING.md](CONTRIBUTING.md#english).

The Wiki
--------

The GitHub project page has a [wiki][] that you should feel free to edit.  The wiki lists the features and bugs that are on plan to include in upcoming versions of PHP Mode.  It is also a place to add any tips to make the mode more useful.

## Copyright

PHP Mode is licensed under [GNU General Public License Version 3][gpl-v3] (GPLv3).

This project originated in `php-mode.el` written by [Turadg Aleahmad][@turadg] in 1999.  In 2013 [Daniel Hackney][@haxney] began rewriting parts of PHP Mode in terms of Emacs' built-in CC Mode.  Other contributors are listed in [Authors] and [Contributors].

This project was maintained by [Eric James Michael Ritz][@ejmr] until 2017. Currently, the [Friends of Emacs-PHP Development][@emacs-php] community inherits PHP Mode.

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
[wiki-manual-installation]: https://github.com/emacs-php/php-mode/wiki/Manual-installation
