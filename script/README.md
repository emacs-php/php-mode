
# Maintenance script for PHP Mode

These scripts are provided for maintenance of PHP Mode.

## Extract PHP Functions

This script extract PHP function names from <http://doc.php.net/downloads/json/php_manual_en.json>.

### Usage

```
Usage:
	./script/extract_functions.php count < php_manual_en.json
	./script/extract_functions.php modules < php_manual_en.json
	./script/extract_functions.php functions < php_manual_en.json > result.json
	./script/extract_functions.php functions-sexp < php_manual_en.json > result.el
```

### Data

A pattern for grouping function modules by function-id is written in `data/module_id_prefixes.php`.

 * All entries are combined as prefix matching patterns
 * The meanings of `.` and `\.` (matches) in the regular expression are swapped
   * `\.` matches any one character
   * `.` matches only `"."`
 * An alphanumeric-terminated entry requires an exact match
   * Other entries require a [`\b`(word boundary)](https://www.php.net/manual/regexp.reference.escape.php) at the end

```php
return [
    'apache' => [
        'function.apache-', // matches all "apache-" prefixed IDs
        'function.virtual', // matches only "function.virtual"
    ],
    'bcmath' => [
        'function.bc\.+',   // matches "function.bcadd", "function.bccomp", ...etc
    ],
```
