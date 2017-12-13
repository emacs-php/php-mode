PHP Mode for GNU Emacs
======================
[![travis badge][travis-badge]][travis-link] [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

This project updates PHP Mode for GNU Emacs with features to make it more friendly to use with PHP 5.4 and later.  This fork builds on the work of:

1. Turadg Aleahmad (Original Author)
2. Aaron S. Hawley
3. Lennart Borgman

All contributors listed below improved PHP Mode as well.

The current maintainers are:

1. Syohei Yoshida
2. Eric James Michael Ritz
3. USAMI Kenta (@zonuexe)

Please submit any bug reports or feature requests by creating issues on [the GitHub page for PHP Mode](https://github.com/ejmr/php-mode).


Installation
------------

**PHP Mode requires Emacs 24 or later.**  PHP Mode may work with older versions of Emacs but this is not guaranteed.  Bug reports for problems related to using PHP Mode with older versions of Emacs will most like *not* be addressed.

With GNU Emacs 24 or later then you can use its [package][] feature to install PHP Mode from [MELPA][].  *The [Marmalade][] package repository only has the original PHP Mode from 2004.*  Therefore we recommend you use MELPA to install PHP Mode.  If you simply do not wish to use the package manager, then all you need to do is download the `php-mode.el` file, place it inside your `load-path`, and optionally add `(require 'php-mode)` to your Emacs configuration to automatically enable PHP Mode whenever you open a PHP file.

Additionally, you can add `skeleton/php-ext.el` to your `load-path` to [enable the templates](https://www.gnu.org/software/emacs/manual/html_node/autotype/index.html#Top).

```lisp
(eval-after-load 'php-mode
  '(require 'php-ext))
```

Reporting Bugs
--------------

When reporting a bug please run the function `php-mode-version` and include its output in your bug report.  This helps up reproduce any problem you may have.


Experimental and In-Progress Features
-------------------------------------

### CC Mode, CEDET, EDE, and Semantic ###

In 2013 Daniel Haxney began rewriting parts of PHP Mode in terms of Emacs' built-in CC Mode.  This laid the foundation for incorporating some of the inherit IDE-ish features of Emacs, such as CEDET, EDE, and Semantic.  Support for these tools continues to improve thanks to the work of Andrea Turso, Steven Rémot, Joris Steyn, and others.  If you wish to test, contribute to, or simply experiment with such features then [this thread is a good place to start](https://github.com/ejmr/php-mode/issues/256).

### PHP 7 Support ###

PHP 7 has been released.  PHP Mode supports the following features and changes from PHP 7:

1. Type-hints for return values in functions and methods receive syntax highlighting in the same way as type-hints for function and method parameters.

2. PHP Mode treats `yield from` as keyword in the same way it already does for a sole `yield`.

3. It recognizes `strict_types` as a special declaration in the same way as `ticks`.


Features
--------

### New Keywords ###

Now PHP Mode supports syntax highlighting for new keywords which PHP 5.4 introduced, e.g. those related to traits, such as `insteadof`.  Also supported are the older keywords `clone` and `default`.

### Constants ###

Syntax highlighting includes every magic constant and predefined constant listed on the official PHP site.  However, some constants from specific extensions are not currently included.

### Traits, Interfaces, and Namespaces ###

Traits, interfaces, and namespaces now appear under Imenu listings. Fontification behaves properly for namespaces as well, so that code like `namespace Foo\Bar\Baz` no longer looks like a warning.  This is also true for namespace aliases, e.g. `use <namespace> as <alias>`; currently the aliased name is not listed in Imenu, but future versions will address this.

### Treatment of Underscores ###

PHP Mode treats underscores as ‘symbol constituents’ (in Emacs terminology) so that you can use keys like `M-f` and `M-b` to move through the individual parts of a variable name like `$foo_bar_baz`.

### Chained Method Calls ###

PHP Mode can align method calls over multiple lines anchored around the `->` operator, e.g.:

```php
$object->foo()
       ->bar()
       ->baz();
```

This behaviour is off by default, but you can customize the variable `php-lineup-cascaded-calls` to enable this.

**Note:** Alignment will only work if you use one of the php-mode coding styles or inherit one of the styles.

### Nested Array Formatting ###

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

### Anonymous Functions ###

Anonymous functions such as

```php
$greet = function($name) { ... };
```

will now appear on Imenu; in this case the name will be `$greet`.

### Flymake Support ###

By customizing the variable `php-executable` you can enable Flymake mode in order to see warnings and errors in real-time as you write code.

### Search Local Documentation ###

The key command `C-c C-f` will search the PHP website for documentation on the word under the cursor.  However, if you have a [local copy of the PHP documentation](http://us2.php.net/download-docs.php) then PHP Mode will try searching that documentation first.  All you need to do is customize the variable `php-manual-path` and give it the path to your copy of the documentation.  If PHP Mode cannot find something locally then it will still fallback on searching the PHP website.

### Executing Regions of PHP ###

The command `php-send-region`, which is bound to `C-c C-r` by default, will execute the selected region of PHP code.  In conjunction with the Emacs command `C-x h` you can use this to execute an entire file.  Any output will appear in a buffer called `*PHP*`.

### PHPDoc Tag / Annotation Highlighting ###

PHPDoc is a documentation format similar to [JavaDoc](https://en.wikipedia.org/wiki/Javadoc).

There are `@param`, `@return`, `@var`... etc in the notation called **tag**, look at [list of tags defined by phpDocumentor2](https://phpdoc.org/docs/latest/references/phpdoc/tags/index.html).  (These tags are compatible with static type checkers like PhpStorm and [Phan](https://github.com/etsy/phan).)

In addition, it also partially supports notation called **annotation**.  Annotation has a slightly different grammar from tag, and the example is `@Annotation(attr1="vvv", attr2="zzz")`.

[Symfony](http://symfony.com/) project and [Go! AOP](https://github.com/goaop/framework) and some projects/frameworks use annotation grammer based on [Doctrine Annotations](http://docs.doctrine-project.org/projects/doctrine-common/en/latest/reference/annotations.html).

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

PHP Mode has not fully supported [PSR-5: PHPDoc (Draft)](https://github.com/phpDocumentor/fig-standards/blob/master/proposed/phpdoc.md) yet.

### Coding Styles ###

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

#### Symfony2 Style ####

With this style method call chains can be formatted with indented continuation and a hanging semi-colon:

```php
    $user1
        ->setCreateDate(new \DateTime('2007-05-07 01:34:45'))
        ->setLastDate(new \DateTime('2012-08-18 19:03:02'))
        ->setUsername('jay')
    ;
```

This style is used widely throughout Symfony2 source code even if it is not explicitly mentioned in their conventions documents.

### Extra Constants ###

If you commonly use a framework or library that defines a set of constants then you may wish to customize the value of `php-extra-constants`.  It is a list of strings that PHP Mode will treat as additional constants, i.e. providing them the same level syntax highlighting that PHP Mode uses for built-in constants.

### Web Mode Constants and Keywords ###

If you use [Web Mode][] then PHP Mode will attempt to use any additional PHP constants and keywords that Web Mode allows you to define.

### Avoid HTML Template Compatibility ###

Many developers use PHP Mode to edit pure PHP scripts (e.g. files with only PHP and no HTML). A basic compatibility layer with HTML has historically been part of PHP Mode but it does not work perfectly and can cause some bad side effects such as slowness and incorrect font locking.  Configuring the `php-template-compatibility` property with a `nil` will cancel any attempt of HTML compatibility.  [Web Mode](http://web-mode.org/) is a great alternative to PHP Mode if you need to work with PHP scripts that do contain HTML and other markup.

### Subword Mode ###

GNU Emacs comes with [Subword Mode][], a minor mode that allows you to navigate the parts of a [camelCase][] as if they were separate words.  For example, PHP Mode treats the variable `$fooBarBaz` as a whole name by default.  But if you enable Subword Mode then Emacs will treat the variable name as three separate words, and therefore word-related commands (e.g. `M-f`, `M-b`, `M-d`) will only affect the camelCase part of the name under the cursor.

If you want to always use Subword Mode for PHP files then you can add this to your Emacs configuration:

```lisp
(add-hook 'php-mode-hook (lambda () (subword-mode 1)))
```

The key-binding `C-c C-w` will also toggle Subword Mode on and off.

### Amaka Support ###

Viewing and editing build scripts for [Amaka](http://trashofmasters.github.io/amaka/) will automatically enable PHP Mode.

### Insert current class/namespace ###

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

All contributions to PHP Mode are welcome.  But please try to do the following when sending improvements or bug fixes:

1. Add your name to the list of ‘Contributors’ in this `README.md` file if it is not there already.  If you have a GitHub page and/or personal site then please feel free to link your name to it so people can see your other work.

2. If your contribution addresses an issue on the GitHub project page then include a single line like `GitHub-Issue: #16` with the appropriate issue number.

3. Make sure to update the constant `php-mode-modified` *only if you patch affects `php-mode.el`,* which means this step is unnecessary for patches related to unit tests.

4. However, please do not modify `php-mode-version-number`.  The maintainers will decide what constitutes a bump in the version number.

5. Open the `php-mode-test.el` file and [run all of the tests](http://www.gnu.org/software/emacs/manual/html_node/ert/Running-Tests-Interactively.html#Running-Tests-Interactively) to ensure they still pass as expected.  Sometimes we expect for a test to fail, and those unit tests have the appropriate configuration so their failure will not raise any warnings.  You can use `make test` script to run all tests from a terminal, which is also useful in conjunction with [`git bisect run`](http://git-scm.com/book/en/Git-Tools-Debugging-with-Git).

6. Send us a pull request here on GitHub.

7. Please make your commit messages as detailed as possible.  It is better to be too verbose than to write too little.  Look at the commits of the maintainers to see many examples of the level of detail that we feel is ideal.  Please never assume that your patch is so simple that future developers will be able to understand the *reason* for the change without comment.  And that is important: your commit message should always strive to answer *"Why"* the patch exists, *"What*" does it accomplish?  The maintainers will sometimes write detailed commit messages for pull-requests by other developers, but please do not rely on us to do this consistently.

If you are fixing a bug related to a GitHub issue, then first of all, thank you for the help improving PHP Mode.  Second, there is a `tests/` directory which contains PHP scripts for issues (although not all of them).  Please consider adding a test script to that directory that documents the expected behavior and provides code that allows others to see if said behavior works properly.  Then create a unit test within `php-mode-test.el` using [ERT][]. Please try to follow the format of the existing tests.


The Wiki
--------

The GitHub project page has a [wiki][] that you should feel free to edit.  The wiki lists the features and bugs that are on plan to include in upcoming versions of PHP Mode.  It is also a place to add any tips to make the mode more useful.


The Mailing List
----------------

The “emacs-php” mailing list is a place to discuss PHP Mode as well as all other PHP-related packages for Emacs.  You can find the mailing list at:

1. [emacs-php at Google Groups](https://groups.google.com/forum/#!forum/emacs-php)
2. [Gmane](http://dir.gmane.org/gmane.emacs.php)

We encourage all users of PHP Mode *and* developers of any PHP-related packages to feel free to post anything there regarding PHP and Emacs.


License
-------

PHP Mode uses the [GNU General Public License 3](http://www.gnu.org/copyleft/gpl.html).


Contributors
------------

In chronological order:

1. Juanjo
2. Torsten Martinsen
3. Vinai Kopp
4. Sean Champ
5. Doug Marcey
6. Kevin Blake
7. Rex McMaster
8. Mathias Meyer
9. Boris Folgmann
10. Roland
11. Rosenfeld
12. Fred Yankowski
13. Craig Andrews
14. John Keller
15. Ryan
16. Sammartino
17. ppercot
18. Valentin Funk
19. Stig Bakken
20. Gregory Stark
21. Chris Morris
22. Nils Rennebarth
23. Gerrit Riessen
24. Eric Mc Sween
25. Ville Skytta
26. Giacomo Tesio
27. Urban Müller
28. [Engelke Eschner](https://github.com/tekai)
29. Lennart Borgman
30. Stefan Monnier
31. Aaron S. Hawley
32. [Ian Eure](https://github.com/ieure)
33. [Bill Lovett](https://github.com/lovett)
34. Dias Badekas
35. David House
36. [Tom Willemse](https://github.com/ryuslash)
37. [Olaf the Viking](https://github.com/olavTHEviking)
38. [Maël Nison](https://github.com/arcanis)
39. [flack](https://github.com/flack)
40. [Michele Bini](https://github.com/rev22)
41. Emanuele Tomasi
42. [David Maus](https://github.com/dmj)
43. [Jakub Jankiewicz](https://github.com/jcubic)
44. [Marcin Antczak](https://github.com/marcinant)
45. [顾伟刚](https://github.com/guweigang)
46. [zapad](https://github.com/zargener)
47. [Carl Groner](https://github.com/cgroner)
48. [Michael Dwyer](https://github.com/kalifg)
49. [Daniel Hackney](https://github.com/haxney)
50. [Nate Eagleson](https://github.com/NateEag)
51. [Steve Purcell](https://github.com/purcell)
52. TatriX
53. [François-Xavier Bois](https://github.com/fxbois)
54. [James Laver](https://github.com/jjl)
55. [Jacek Wysocki](https://github.com/exu)
56. [Jon Dufrense](https://github.com/jdufresne)
57. [Andrei Chițu](https://github.com/achitu)
58. [phil-s](https://github.com/phil-s)
59. [Bence Kalmar](https://github.com/brkalmar)
60. [Elis Axelsson](https://github.com/etu)
61. [Alan Pearce](https://github.com/alanpearce)
62. Syohei Yoshida
63. Joris Steyn
64. l3msh0
65. [Hernawan Fa'iz Abdillah](https://github.com/Abdillah)
66. [Sebastian Wiesner](https://github.com/lunaryorn)
67. [Michael Stolovitzsky](https://github.com/emestee)
68. [David Arroyo Menéndez](https://github.com/davidam)
69. [USAMI Kenta](https://tadsan.github.io/) (@zonuexe)
70. [Tim Landscheidt](http://www.tim-landscheidt.de)
71. [Fabian Wiget](https://github.com/fabacino)
72. tangxifan
73. [Serghei Iakovlev](https://github.com/sergeyklay)
74. [Christian Albrecht](https://github.com/calbrecht)



[wiki]: https://github.com/ejmr/php-mode/wiki
[ert]: http://www.gnu.org/software/emacs/manual/html_node/ert/index.html
[cc mode]: https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
[Subword Mode]: https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
[camelCase]: http://en.wikipedia.org/wiki/Camel_case
[package]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html
[MELPA]: http://melpa.milkbox.net/
[Marmalade]: http://marmalade-repo.org/
[Web Mode]: http://web-mode.org/
[travis-badge]: https://travis-ci.org/ejmr/php-mode.svg
[travis-link]: https://travis-ci.org/ejmr/php-mode
[melpa-link]: http://melpa.org/#/php-mode
[melpa-stable-link]: http://stable.melpa.org/#/php-mode
[melpa-badge]: http://melpa.org/packages/php-mode-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/php-mode-badge.svg
