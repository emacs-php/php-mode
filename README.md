# php-mode

This updates `php-mode` with features to make it more friendly to use with PHP 5.4 and 5.5.  It is based on the work of:

1. Turadg Aleahmad: The original author.

2. Aaron S. Hawley

3. Lennart Borgman

And all those mentioned in the ‘Contributors’ section below.

Please email any bugs or feature requests to `lobbyjones at gmail dot com` or submit them as Issues here on the Github page.  Also please include the output of `php-mode-version` in bug reports.

**Note:** [You can see all of the previous changes for each version here.](https://github.com/ejmr/php-mode/blob/master/Changelog.md)

# Status

**2 December 2012:**  Currently I am not working on any major PHP projects.  However, I do keep up with the development of PHP (i.e. following the official mailing list).  With PHP 5.5 on the horizon I will be adding basic support for new 5.5 features in the near future.  That also goes for anything promising that appears in version 5.6.0-dev, which I use for testing.  And as always I welcome any feature additions and bug fixes from users.

# Features

## New Keywords

Now `php-mode` supports syntax highlighting for new keywords which were added as part of PHP 5.4, e.g. those related to traits, such as `insteadof`.  Also supported are the older keywords `clone` and `default`.

## Constants

Every magic constant and predefined constant listed on the official PHP site is included in syntax highlighted.

## Traits, Interfaces, and Namespaces

Traits, interfaces, and namespaces now appear under Imenu listings.  Fontification works properly for namespaces as well, so that code like `namespace Foo\Bar\Baz` no longer looks like a warning.  This is also true for namespace aliases, e.g. `use <namespace> as <alias>`; currently the aliased name is not listed in Imenu, but that will be addressed in the future.

## Treatment of Underscores

Underscores are treated as ‘symbol constituents’ (in Emacs terminology) so that you can use keys like M-f and M-b to move through the individual parts of a variable name like `$foo_bar_baz`.

## Chained Method Calls

A series of method calls over multiple lines will now be aligned with the `->` operator, e.g.:

    $object->foo()
           ->bar()
           ->baz();

## Nested Array Formatting

Nested function calls and `array()` structures now look better by default (or at least in my opinion).  Here is an example of the style:

    $results = Post::model()->find(
        array(
            'select' => 'title',
            'condition' => 'postID=:postID',
            'params' => array(':postID' => 10),
        )
    );

## Anonymous Functions

Anonymous functions such as

    $greet = function($name) { … };

will now appear on Imenu; in this case the name will be `$greet`.

## Flymake Support

By customizing the variable `php-executable` you can enable Flymake mode in order to see warnings and errors in real-time as you write code.

## Search Local Documentation

The key command `C-c C-f` will search the PHP website for documentation on the word under the cursor.  However, if you have a [local copy of the PHP documentation](http://us2.php.net/download-docs.php) then `php-mode` will try searching that documentation first.  All you need to do is customize the variable `php-manual-path` and give it the path to your copy of the documentation.  If `php-mode` cannot find something locally then it will still fallback on searching the PHP website.

## Executing Regions of PHP

The command `php-send-region`, which is bound to `C-c C-r` by default, will execute the selected region of PHP code.  In conjunction with the Emacs command `C-x h` you can use this to execute an entire file.  Any output will appear in a buffer called `*PHP*`.

## Annotation Highlighting

Projects like [Symfony](http://symfony.com/) use annotations in comments.  For example, here is code from their website:

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

The annotations are the lines that begin with the `@` character, and `php-mode` will give these special highlighting to help them stand out.

## Coding Styles

By default `php-mode` tries to provide a reasonable style for indentation and formatting.  However, it provides other options suited for particular projects which you may find useful.  These coding styles are available through three functions:

1. `php-enable-pear-coding-style`
2. `php-enable-drupal-coding-style`
3. `php-enable-wordpress-coding-style`

They will help format your code for PEAR projects, or work on the Drupal and WordPress software, respectively.  You may enable any of them by default by running `M-x customize-group <RET> php` and looking for the ‘PHP Mode Coding Style’ option.

# How to Contribute

All contributions to `php-mode` are welcome.  But please try to do the following when sending improvements or bug fixes:

1. Add your name to the list of ‘Contributors’ in this `README.md` file if it is not there already.  If you have a Github page then please link your name to it, so people can see your other work.

2. If your contribution addresses an issue on the Github project page then include a single line like `Github-issue: 16` with the appropriate issue number.

3. Make sure to update the constant `php-mode-modified`.

4. However, please do not modify `php-mode-version-number`.  I will decide what constitutes a bump in the version number.

5. Send me a pull request here on Github.  Or if you do not have a Github account then email the patches to me at `lobbyjones at gmail dot com`.  Please try to make sure the patches are acceptable input to the comand `git am`.  Please note that even if you send a pull request it is very likely that I will *not* simply merge your branch through Github; I prefer to go through commits and cherry-pick them so I can review the commit messages and sign-off on them.  You can see which commits I did or did not merge by using the [`git-cherry`](http://www.kernel.org/pub/software/scm/git/docs/git-cherry.html) command.

If you are fixing a bug related to a Github issue, then first of all, thank you for the help improving `php-mode`.  Second, there is a `tests/` directory which contains PHP scripts for each issue (although not all of them).  Please consider adding a test script to that directory that documents the expected behavior and provides code that allows others to see if said behavior works properly.  Please try to follow the format of the existing tests.

# The Wiki

The Github project page [has a wiki](https://github.com/ejmr/php-mode/wiki) that you should feel free to edit.  The wiki lists the features and bugs that are on plan to include in upcoming versions of `php-mode`.  It is also a place to add any tips to make the mode more useful.

# License

The code for `php-mode` is covered by the [GNU General Public License 3](http://www.gnu.org/copyleft/gpl.html).

# Contributors

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
27. Lennart Borgman
28. Stefan Monnier
29. Aaron S. Hawley
30. [Ian Eure](https://github.com/ieure)
31. [Bill Lovett](https://github.com/lovett)
32. Dias Badekas
33. David House
34. [Tom Willemsen](https://github.com/ryuslash)
35. [Olaf the Viking](https://github.com/olavTHEviking)
36. [Maël Nison](https://github.com/arcanis)
37. [flack](https://github.com/flack)
38. [Michele Bini](https://github.com/rev22)
39. Emanuele Tomasi
40. [David Maus](https://github.com/dmj)
41. [Jakub Jankiewicz](https://github.com/jcubic)
42. [Marcin Antczak](https://github.com/marcinant)
43. [顾伟刚](https://github.com/cnwggu)
