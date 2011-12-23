# php-mode

This updates `php-mode` with features to make it more friendly to use with PHP 5.4.  It is based on the work of:

1. Turadg Aleahmad

2. Aaron S. Hawley

3. Lennart Borgman

And all those mentioned in the ‘Contributors’ section below.

Please email any bugs or feature requests to `Ren at lifesnotsimple dot com` or submit them as Issues here on the Github page.  Also please include the output of `php-mode-version` in bug reports.

# Status

**23 December 2011:** These days I am doing less and less PHP programming, and thus work on `php-mode` has slowed down.  However I am still accepting bug reports and feature requests, so please do not consider the project dead.  I have a list of features I would like to add, but right now I am unsure when I will get around to them.  My apologies for the slow updates.

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
            'select'    => 'title',
            'condition' => 'postID=:postID',
            'params'    => array(':postID' => 10),
        )
    );

(Note: The alignment of the `=>` operators is not part of these changes.)

## Anonymous Functions

Anonymous functions such as

    $greet = function($name) { … };

will now appear on Imenu; in this case the name will be `$greet`.

## Flymake Support

By customizing the variable `php-executable` you can enable Flymake mode in order to see warnings and errors in real-time as you write code.

## Search Local Documentation

The key command `C-c C-f` will search the PHP website for documentation on the word under the cursor.  However, if you have a [local copy of the PHP documentation](http://us2.php.net/download-docs.php) then `php-mode` will try searching that documentation first.  All you need to do is customize the variable `php-manual-path` and give it the path to your copy of the documentation.  If `php-mode` cannot find something locally then it will still fallback on searching the PHP website.

# License

The code for `php-mode` is covered by the GNU General Public License 3.

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
30. Ian Eure
31. Bill Lovett
32. Dias Badekas
33. David House
34. Tom Willemsen
35. Olaf the Viking
