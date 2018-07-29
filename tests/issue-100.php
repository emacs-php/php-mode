<?php

/**
 * GitHub Issue:    https://github.com/emacs-php/php-mode/issues/100
 *
 * The code below should appear like so:
 *
 *     use ReflectionMethod,
 *         ReflectionObject,
 *         ReflectionProperty;
 *
 * Each namespace must have the correct font-face.  Aligning the
 * namespaces is also required to satisfy the test.
 * 
 */

use ReflectionMethod,
    ReflectionObject,
    ReflectionProperty;

class Foo
{
    use ReflectionMethod,
        ReflectionObject,
        ReflectionProperty;
}