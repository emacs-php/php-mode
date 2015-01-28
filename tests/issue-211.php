<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/211
 *
 * Indentation of string concatination
 */

// Start:
$str = 'some'
. 'string1';

$str_long_name = 'some'
                             . 'string2';

// Multiple lines
$sql = "SELECT `id`, `name` FROM `people` "
     . "WHERE `name` = 'Susan' "
. "ORDER BY `name` ASC ";
