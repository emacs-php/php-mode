<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/124
 *
 * PHP Mode must highlight the heredoc string below using
 * font-lock-string-face.  The presence of the apostrophe or anything
 * else within the string should not stop the highlighting from
 * leaking out beyond the ending 'EOT' marker.
 * 
 */

$foo = <<<EOT
Start of heredoc with a ' inside.
EOT;

function bar()
{
    return true;
}
