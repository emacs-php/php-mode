<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/186
 *
 * Test indentation of case body preceeded by multiple case labels
 * that fall through
 *
 * Note that this test uses the PEAR standard where case and switch
 * statements are aligned to the same column, unlike the other styles.
 * This does not matter for the purpose of this test.
 */

switch (true) {
case true:
    echo 'test'; // ###php-mode-test### ((indent (* c-basic-offset 2)))
    echo 'test'; // ###php-mode-test### ((indent (* c-basic-offset 2)))
}

switch (true) {
case null:
case false:
    echo 'test'; // ###php-mode-test### ((indent (* c-basic-offset 2)))
    echo 'test'; // ###php-mode-test### ((indent (* c-basic-offset 2)))
}

switch (true) {
case "test":
case "test":
    echo 'test'; // ###php-mode-test### ((indent (* c-basic-offset 2)))
    echo 'test'; // ###php-mode-test### ((indent (* c-basic-offset 2)))
}

switch (true) {
case $test:
case $test:
    echo 'test'; // ###php-mode-test### ((indent (* c-basic-offset 2)))
    echo 'test'; // ###php-mode-test### ((indent (* c-basic-offset 2)))
}
