PHP Mode for GNU Emacs
======================

This updates PHP Mode with features to make it more friendly to use with PHP 5.4 and later.  This fork builds on the work of:

1. Turadg Aleahmad (Original Author)

2. Aaron S. Hawley

3. Lennart Borgman

All contributors listed below improved PHP Mode as well.

Please email any bugs or feature requests to `lobbyjones at gmail dot com` or submit them as Issues on the [GitHub page](https://github.com/ejmr/php-mode).  Also please include the output of `php-mode-version` in bug reports.  [There is a changelog for previous versions.](./Changelog.md)


Installation
------------

If you are using GNU Emacs 24 or later then you can use its [package][] feature to install PHP Mode from [MELPA][].  *The [Marmalade][] package repository only has the original PHP Mode from 2004.*

If you are using an older version of Emacs, or if you simply do not wish to use the package manager, then all you need to do is download the `php-mode.el` file, place it inside your `load-path`, and optionally add `(require 'php-mode)` to your Emacs configuration to automatically enable PHP Mode whenever you open a PHP file.

**Note:** PHP Mode requires Emacs 23.1 or later.


Status
------

Version 1.13 is the finaly version I intend to release, with the exception of critical bug fixes.  I am currently looking for someone willing to take over as the maintainer of PHP Mode starting in 2014.  Please see [this discussion](https://github.com/ejmr/php-mode/issues/109) for details.


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

PHP Mode will align method calls over multiple lines anchored around the `->` operator, e.g.:

```php
$object->foo()
       ->bar()
       ->baz();
```

**Note:** Alignment will only work if you use one of the coding styles described below.  PHP Mode uses [CC mode][] for indentation.  If you use any indentation style other than those described under the *Coding Styles* section then the method alignment above is not guaranteed to work.

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

### Annotation Highlighting ###

Projects like [Symfony](http://symfony.com/) use annotations in comments.  For example, here is code from their website:

```php
/**
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

### Coding Styles ###

By default PHP Mode tries to provide a reasonable style for indentation and formatting.  However, it provides other options suited for particular projects which you may find useful.  These coding styles are available through these functions:

1. `php-enable-pear-coding-style`
2. `php-enable-drupal-coding-style`
3. `php-enable-wordpress-coding-style`
4. `php-enable-symfony2-coding-style`

They will help format your code for PEAR projects, or work on Drupal, WordPress, and Symfony2 software, respectively.  You may enable any of them by default by running `M-x customize-group <RET> php` and looking for the ‘PHP Mode Coding Style’ option.

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


How to Contribute
-----------------

All contributions to PHP Mode are welcome.  But please try to do the following when sending improvements or bug fixes:

1. Add your name to the list of ‘Contributors’ in this `README.md` file if it is not there already.  If you have a GitHub page then please link your name to it, so people can see your other work.

2. If your contribution addresses an issue on the GitHub project page then include a single line like `GitHub-Issue: 16` with the appropriate issue number.

3. Make sure to update the constant `php-mode-modified`.

4. However, please do not modify `php-mode-version-number`.  I will decide what constitutes a bump in the version number.

5. Open the `php-mode-test.el` file and [run all of the tests](http://www.gnu.org/software/emacs/manual/html_node/ert/Running-Tests-Interactively.html#Running-Tests-Interactively) to ensure they still pass as expected.  Sometimes we expect for a test to fail, and those unit tests have the appropriate configuration so their failure will not raise any warnings.  You can use `make test` script to run all tests from a terminal, which is also useful in conjunction with [`git bisect run`](http://git-scm.com/book/en/Git-Tools-Debugging-with-Git).

6. Send me a pull request here on GitHub.  Or if you do not have a GitHub account then email the patches to me at `lobbyjones at gmail dot com`.  Please try to make sure the patches are acceptable input to the comand `git am`.  Please note that even if you send a pull request it is very likely that I will *not* simply merge your branch through GitHub; I prefer to go through commits and cherry-pick them so I can review the commit messages and sign-off on them.  You can see which commits I did or did not merge by using the [`git-cherry`](http://www.kernel.org/pub/software/scm/git/docs/git-cherry.html) command.

If you are fixing a bug related to a GitHub issue, then first of all, thank you for the help improving PHP Mode.  Second, there is a `tests/` directory which contains PHP scripts for issues (although not all of them).  Please consider adding a test script to that directory that documents the expected behavior and provides code that allows others to see if said behavior works properly.  Then create a unit test within `php-mode-test.el` using [ERT][]. Please try to follow the format of the existing tests.


The Wiki
--------

The GitHub project page has a [wiki][] that you should feel free to edit.  The wiki lists the features and bugs that are on plan to include in upcoming versions of PHP Mode.  It is also a place to add any tips to make the mode more useful.


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
45. [顾伟刚](https://github.com/cnwggu)
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



[wiki]: https://github.com/ejmr/php-mode/wiki
[ert]: http://www.gnu.org/software/emacs/manual/html_node/ert/index.html
[cc mode]: https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
[Subword Mode]: https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
[camelCase]: http://en.wikipedia.org/wiki/Camel_case
[package]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html
[MELPA]: http://melpa.milkbox.net/
[Marmalade]: http://marmalade-repo.org/
[Web Mode]: http://web-mode.org/
