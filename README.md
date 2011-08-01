# php-mode

This updates `php-mode` with features to make it more friendly to use with PHP 5.4.  It is based on the work of:

1. Turadg Aleahmad

2. Aaron S. Hawley

3. Lennart Borgman

And all those mentioned in the ‘Contributors’ section below.

Please email any bugs or feature requests to `Ren at lifesnotsimple dot com`.

# Features

## New 5.4 Keywords

Now `php-mode` supports syntax highlighting for new keywords which were added as part of PHP 5.4, e.g. those related to traits, such as `insteadof`.

## Traits and Namespaces

Traits and namespaces now appear under Imenu listings.  Fontification works properly for namespaces as well, so that code like `namespace Foo\Bar\Baz` no longer looks like a warning.  This is also true for namespace aliases, e.g. `use <namespace> as <alias>`; currently the aliased name is not listed in Imenu, but that will be addressed in the future.

## Treatment of Underscores

Underscores are treated as ‘symbol constituents’ (in Emacs terminology) so that you can use keys like M-f and M-b to move through the individual parts of a variable name like `$foo_bar_baz`.

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

