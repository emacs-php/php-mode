<?php

/**
 * GitHub Issue:  https://github.com/emacs-php/php-mode/issues/439
 */

$a = <<<ABC
Let'go Justin
ABC;

$b = <<<A_B_C
Let'go Justin
A_B_C;

$c = <<<'A_B_C'
Let'go Justin
A_B_C;

$d = <<<"A_B_C"
Let'go Justin
A_B_C;

$e = <<<いろは
Let'go Justin
いろは;

$f = <<<'いろは'
Let'go Justin
いろは;

$g = <<<"いろは"
Let'go Justin
いろは;

var_dump(<<<"ABC"
Let'go Justin
ABC);

if (1 === 1) {
    var_dump(<<<"ABC"
    Let'go Justin
    ABC);
}
