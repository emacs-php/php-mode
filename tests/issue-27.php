#!/usr/bin/env php
<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/27
 *
 * The file tests indentation on a script beginning with a shebang.
 * If you place the cursor on the line '// Tab' and indent it should
 * move to the right the appropriate number of spaces (or one tab stop
 * if that is your configuration).  The bug report linked above shows
 * that this indentation does not always work.
 */

if (1 == 1) {
    // Tab ###php-mode-test### ((indent c-basic-offset))
}
