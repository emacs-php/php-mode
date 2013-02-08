<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/29
 *
 * The mode should format the code below like so:
 *
 * $app->get("/index", function() {
 *     $app->redirect("foo");
 * });
 *
 */

$app->get("/index", function() {
    $app->redirect("foo"); // ###php-mode-test### ((indent c-basic-offset))
});                        // ###php-mode-test### ((indent 0))
