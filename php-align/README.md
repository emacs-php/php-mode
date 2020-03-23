# php-align.el

CAUTION!! this is still experimental.

Support alignment (e.g. `align`, `align-current`) for PHP.

Put this file into your load-path.and the following code into your ~/.emacs

```el
(add-hook 'php-mode-hook #'php-align-setup)
```

## Examples

### 1.

#### before

```php
$foo = "string"; // M-x align-current
$looooooooong = 1; //
```

#### after

```php
$foo          = "string"; // M-x align-current
$looooooooong = 1;        //
```

### 2.

#### before

```php
"$foo = 1";
$foo = "string"; // M-x align-current
$looooooooong = 1; //

$bar = 2; //
```

#### after

```php
"$foo = 1";
$foo          = "string"; // M-x align-current
$looooooooong = 1;        //

$bar = 2; //
```

### 3.

#### before

```php
$variable = 1;
$vars = array(); // M-x align-current
if ($variable == $vars) {

}
```

#### after

```php
$variable = 1;
$vars     = array(); // M-x align-current
if ($variable == $vars) {

}
```

### 4.

#### before

```php
$vars = array(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9,
    10, 11, 12, // C-u M-x align-current
);
```

#### after

```php
$vars = array(
    1,  2,  3,
    4,  5,  6,
    7,  8,  9,
    10, 11, 12, // C-u M-x align-current
);
```
