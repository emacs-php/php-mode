<?php declare(strict_types=1); ?>
/** GitHub Issue: https://github.com/emacs-php/php-mode/issues/443 */
<?xml version="1.0" encoding="UTF-8"?>
<?hh echo "Hack open tag" ?>
<? echo "obsolete short open tag" ?>
<% echo "obsolete ASP tags" %>
<?= "foo" ?>
<?= 1 + 1 ?>
<?php /* Foo */ echo "Bar"; ?>
<?php/* Foo */ echo "Bar"; ?>
