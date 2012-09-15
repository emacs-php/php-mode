<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues16
 *
 * The third 'use' statement, with comma-separated namespaces, should
 * appear in the same syntax highlighting as the first two 'use'
 * statements that import individual namespaces.  This tests for a
 * previously existing bug where the third line would trigger the
 * warning font face.
 */

use ReflectionMethod;
use ReflectionObject;

use ReflectionMethod, ReflectionObject;