# About #
CAUTION!! this is still experimental.

Support alignment (e.g. align, align-current) for PHP.

Put this file into your load-path.and the following code into your ~/.emacs

<code>
(add-hook 'php-mode-hook
          (lambda ()
            (require 'php-align)
            (php-align-setup)))
</code>

# Examples #

## 1. ##

### before ###

    $foo = "string"; // M-x arign-current
    $looooooooong = 1; //

### after ###

    $foo          = "string"; // M-x arign-current
    $looooooooong = 1;        //

## 2. ##

### before ###

    "$foo = 1";
    $foo = "string"; // M-x arign-current
    $looooooooong = 1; //

    $bar = 2; //

### after ###

    "$foo = 1";
    $foo          = "string"; // M-x arign-current
    $looooooooong = 1;        //

    $bar = 2; //

## 3. ##

### before ###
    $variable = 1;
    $vars = array(); // M-x align-current
    if ($variable == $vars) {

    }

### after ###
    $variable = 1;
    $vars     = array(); // M-x align-current
    if ($variable == $vars) {

    }

## 4. ##

### before ###
    $vars = array(
        1, 2, 3,
        4, 5, 6,
        7, 8, 9,
        10, 11, 12, // C-u M-x align-current
    );

### after ###
    $vars = array(
        1,  2,  3,
        4,  5,  6,
        7,  8,  9,
        10, 11, 12, // C-u M-x align-current
    );