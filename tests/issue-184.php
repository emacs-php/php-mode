<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/184
 *
 * Test indentation of statements at beginning of line.
 *
 * The termination of a heredoc is always fixed bol, but php-mode must
 * detect that not all statements at column 0 are the end of a
 * heredoc.
 */

function test() {
    $html = <<<html
<a href="#"></a>
html;

return;
}