<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/42
 *
 * Indenting the code below, particularly the closures, should not
 * cause any problems such as the long backtrace reported in the issue
 * URL above.
 */

$di->set('config', function () use ($config) {
    return $config;
});

$di->set('logger', function() use ($config) {
    $filename = date('Ymd');
    $logger = new \Logger($config->application->logger->dir . $filename);
    $logger->setFormat($config->application->logger->format);
    return $logger;
});
