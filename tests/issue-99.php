<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/99
 *
 * The code below contains a 'foreach' without any braces.  We use CC
 * Mode to customize indentation and it treats continued differently
 * depending on whether or not they end with a brace.  This test is to
 * ensure that PHP Mode properly indents 'foreach' statements without
 * any braces.
 *
 */

foreach ($x as $s)
        echo $s; // ###php-mode-test### ((indent c-basic-offset))