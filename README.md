# php-mode

This updates `php-mode` with features to make it more friendly to use with PHP 5.4.  It is based on the work of:

1. Turadg Aleahmad

2. Aaron S. Hawley

3. Lennart Borgman

And all those mentioned in the ‘Contributors’ section below.

Please email any bugs or feature requests to `lobbyjones at gmail dot com` or submit them as Issues here on the Github page.  Also please include the output of `php-mode-version` in bug reports.

# Status

**28 July 2012:**  I will not be working on any PHP projects in the forseeable future.  Therefore I have not worked on adding any new features.  However, the project is not dead.  I am still accepting bug requests and will try to address them in a timely manner.  I also welcome any improvements in functionality from other developers.  Updates to `php-mode` may be infrequent, but I will continue to maintain it as best as possible.

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

## Executing Regions of PHP

The command `php-send-region`, which is bound to `C-c C-r` by default, will execute the selected region of PHP code.  In conjunction with the Emacs command `C-x h` you can use this to execute an entire file.  Any output will appear in a buffer called `*PHP*`.

# How to Contribute

All contributions to `php-mode` are welcome.  But please try to do the following when sending improvements or bug fixes:

1. Add your name to the list of ‘Contributors’ in this `README.md` file if it is not there already.

2. If your contribution addresses an issue on the Github project page then include a single line like `Github-issue: 16` with the appropriate issue number.

3. Make sure to update the constant `php-mode-modified`.

4. However, Please do not modify `php-mode-version-number`.  I will decide what constitutes a bump in the version number.

5. Send me a pull request here on Github.  Or if you do not have a Github account then email the patches to me at `lobbyjones at gmail dot com`.  Please try to make sure the patches are acceptable input to the comand `git am`.

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
30. Ian Eure
31. Bill Lovett
32. Dias Badekas
33. David House
34. Tom Willemsen
35. Olaf the Viking
36. Maël Nison
37. flack
