<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/14
 *
 * The code below should be formatted like so if we run indent-region
 * on it:
 *
 * $post = Post::model()->find(array(
 *     'select' => 'title',
 *     'condition' => 'postID=:postID',
 *     'params' => array(':postID'=>10),
 * ));
 *
 */

$post = Post::model()->find(array(
    'select' => 'title',              // ###php-mode-test### ((indent c-basic-offset))
    'condition' => 'postID=:postID',  // ###php-mode-test### ((indent c-basic-offset))
    'params' => array(':postID'=>10), // ###php-mode-test### ((indent c-basic-offset))
));                                   // ###php-mode-test### ((indent 0))