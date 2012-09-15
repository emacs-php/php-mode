<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/9
 *
 * The HTML below contains a single quote as part of "We'll".  The bug
 * for issue 9 shows how the presence of this single quote makes
 * php-mode think that the rest of the line constitutes a quoted
 * string, thus messing up the syntax highlighting of the HTML.  If
 * php-mode is working properly then the contents of the tag should
 * appear in the same highlighting as the initial 'We', and the
 * closing </a> tag should match the highlighting of its opening tag.
 */

?>
<a href="somewhere">We'll go there</a>