<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/237
 *
 * Indentation of chaining method for PSR2
 */

$provider = $this->di->get('provider')
->filterByLanguage('en'); // ###php-mode-test### ((indent c-basic-offset))

function foo() {
    if (true) {
        $provider = $this->di->get('provier')
->filterByLanguage('en'); // ###php-mode-test### ((indent (+ c-basic-offset 8)))
    }
}