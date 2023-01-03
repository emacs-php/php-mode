#!/usr/bin/env php
<?php

/**
 * Extract PHP Functions
 *
 * @copyright 2023 Friends of Emacs-PHP development
 * @author USAMI Kenta <tadsan@zonu.me>
 * @license FSFAP https://spdx.org/licenses/FSFAP.html
 */

// Copying and distribution of this file, with or without modification,
// are permitted in any medium without royalty provided the copyright
// notice and this notice are preserved.  This file is offered as-is,
// without any warranty.

declare(strict_types=1);

error_reporting(E_ALL);

$functions = json_decode(stream_get_contents(STDIN), true);

$command = $argv[0];
$subcommand = $argv[1] ?? null;

$subcommands = [
    'count' => function (array $extracted) {
        echo json_encode(array_map(count(...), $extracted), JSON_PRETTY_PRINT), PHP_EOL;
    },
    'modules' => function (array $extracted) {
        echo implode(PHP_EOL, array_keys($extracted)), PHP_EOL;
    },
    'functions' => function (array $extracted) {
        echo json_encode(array_map(array_keys(...), $extracted), JSON_PRETTY_PRINT), PHP_EOL;
    },
    'functions-txt' => function (array $extracted) {
        foreach ($extracted as $functions) {
            foreach ($functions as $name => $_) {
                echo $name, PHP_EOL;
            }
        }
    },
    'functions-sexp' => function (array $extracted) {
        echo "  '(";
        foreach ($extracted as $module => $functions) {
            echo "\n    ({$module}";
            //            ksort($functions);
            foreach ($functions as $name => $function) {
                $escaped_name = strtr($name, ['\\' => '\\\\']);
                echo "\n     \"{$escaped_name}\"";
            }
            echo ")";
        }
        echo ")";
    },
];

if (!isset($subcommands[$subcommand])) {
    $json_url = 'http://doc.php.net/downloads/json/php_manual_en.json';
    fwrite(STDERR, implode(PHP_EOL, [
        "[Extract PHP Functions]\n",
        "This script extract PHP function names from <{$json_url}>.\n",
        "Usage:",
        "\t{$command} count < php_manual_en.json",
        "\t{$command} modules < php_manual_en.json",
        "\t{$command} functions < php_manual_en.json > result.json",
        "\t{$command} functions-sexp < php_manual_en.json > result.el",
        '',
    ]));
    exit(1);
}

$module_id_patterns = array_map(
    fn($allowlist) => '/\A(?:' . implode('|', array_map(
        fn($preg) => strtr($preg, ['\.' => '.', '.' => '\.']) .
            (preg_match('/[0-9a-z]\z/', $preg) ? '\z' : '\b'),
        $allowlist)
    ) . ')/',
    include __DIR__ . '/data/module_id_prefixes.php'
);

$extracted = [];

foreach ($functions as $name => $function) {
    if (str_contains($name, '::')) {
        continue;
    }

    $module = get_module($function, $module_id_patterns);

    if ($module === null) {
        fwrite(STDERR, "{$name}: {$function['id']}\n");
    } else {
        $extracted[$module][$name] = $function;
    }
}

ksort($extracted);
array_walk($extracted, function (&$functions) {
    ksort($functions);
});

call_user_func($subcommands[$subcommand], $extracted);

/**
 * @param array{id: non-empty-string} $function
 * @param array<non-empty-string, non-empty-string> $function
 */
function get_module(array $function, array $patterns): ?string
{
    foreach ($patterns as $module => $pattern) {
        if (preg_match($pattern, $function['id'])) {
            return $module;
        }
    }

    return null;
}
