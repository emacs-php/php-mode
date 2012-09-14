<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/21
 *
 * This test contains a multiline string.  Emacs should highlight the
 * string properly even though it contains newline characters.
 */

$test = "This is
         a multiline
         string";