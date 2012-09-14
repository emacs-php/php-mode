<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/22
 *
 * Emacs should highlight all three comments below using
 * font-lock-comment-face.  The first two comments must have the same
 * style and highlighting as the final comment, which contains no
 * string.  This tests for a bug where a string in such comments
 * breaks font highlighting.
 */

# A comment with a "string" inside of it.

# And one using 'single quotes' instead.

# Finally, a comment with no strings.