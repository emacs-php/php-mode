<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/145
 *
 * This tests closure indentation.
 *
 */

function bar() {
    return function() {
        array_filter(function($foo) {
            return array_map(function() use ($foo) {
                return "xxx";
            }, "foo");
        });
    };
}