<?php

/**
 * Github issue:    https://github.com/ejmr/php-mode/issues/38
 *
 * This file provides a test case for heredoc support.  The text
 * for $str should use the proper heredoc face.  The extra $str2
 * is to make sure that the heredoc formatting does not disrupt
 * the indentation for any code that follows.
 */

$str = <<<"EOT"
{$var1} Hello
EOT;

$str2 = "Hello world!";