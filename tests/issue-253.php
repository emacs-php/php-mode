<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/253
 *
 * Highlighting fails after a string with too many escaped quotes
 */

class A {
    function a($x = null) {
        $this->x = $x;
    }

    $STRING = '\'\'\'';

    function b($x = null) {
        $this->x = $x;
    }
}
