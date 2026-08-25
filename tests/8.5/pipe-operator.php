<?php

/**
 * PHP 8.5: the pipe operator `|>'.
 *
 * @see https://www.php.net/releases/8.5/en.php
 */

// The example from the release announcement.
$title = ' PHP 8.5 Released ';

$slug = $title
    |> trim(...)                                     // ###php-mode-test### ((indent 4))
    |> (fn($str) => str_replace(' ', '-', $str))     // ###php-mode-test### ((indent 4))
    |> (fn($str) => str_replace('.', '', $str))      // ###php-mode-test### ((indent 4))
    |> strtolower(...);                              // ###php-mode-test### ((indent 4))

// A pipe starting on the same line as the assignment.
$result = $input |> trim(...)                        // ###php-mode-test### ((indent 0))
    |> strtolower(...);                              // ###php-mode-test### ((indent 4))

// Inside a function body.
function slugify(string $title): string
{
    return $title                                    // ###php-mode-test### ((indent 4))
        |> trim(...)                                 // ###php-mode-test### ((indent 8))
        |> strtolower(...);                          // ###php-mode-test### ((indent 8))
}

// Inside an argument list.
var_dump(
    $title                                           // ###php-mode-test### ((indent 4))
        |> trim(...)                                 // ###php-mode-test### ((indent 8))
        |> strtolower(...),                          // ###php-mode-test### ((indent 8))
);                                                   // ###php-mode-test### ((indent 0))

// Piping into a static method and a method call.
$out = $data
    |> Formatter::normalize(...)                     // ###php-mode-test### ((indent 4))
    |> $encoder->encode(...);                        // ###php-mode-test### ((indent 4))
