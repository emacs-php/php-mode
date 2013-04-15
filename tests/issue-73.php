<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/73
 *
 * The `delete-indentation' function should work properly for PHP.
 * This means modifying the logic of `fixup-whitespace' so that it
 * eliminates spaces before ',', ';', '->' amd '::' and after '->' and
 * '::'.
 */

# Correct
TEST::di->set('config', function () use ($config);

# Test
TEST
::
di->set('config', function () use ($config);

# Test
TEST::di
->
set('config', function () use ($config);

# Test
TEST::di->set('config'
, function () use ($config);

# Test
TEST::di->set('config', function () use ($config)
;
