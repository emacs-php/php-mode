# php-mode

This updates `php-mode` with features to make it more friendly to use with PHP 5.4.  It is based on the work of:

1. Turadg Aleahmad

2. Aaron S. Hawley

3. Lennart Borgman

And all those mentioned in the ‘Contributors’ section of the code.

Please email any bugs or feature requests to `Ren at lifesnotsimple dot com`.

# Features

## New 5.4 Keywords

Now `php-mode` supports syntax highlighting for new keywords which were added as part of PHP 5.4, e.g. those related to traits, such as `insteadof`.  Traits now also appear in Imenu listings, as well as namespaces.

## Treatment of Underscores

Underscores are treated as ‘symbol constituents’ (in Emacs terminology) so that you can use keys like M-f and M-b to move through the individual parts of a variable name like `$foo_bar_baz`.

# License

The code for `php-mode` is covered by the GNU General Public License 3.
