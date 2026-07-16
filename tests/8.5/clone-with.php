<?php

/**
 * PHP 8.5: `clone' with a second argument ("clone with").
 *
 * @see https://www.php.net/releases/8.5/en.php
 */

// The example from the release announcement.
readonly class Color
{
    public function __construct(                     // ###php-mode-test### ((indent 4))
        public int $red,                             // ###php-mode-test### ((indent 8))
        public int $green,                           // ###php-mode-test### ((indent 8))
        public int $blue,                            // ###php-mode-test### ((indent 8))
        public int $alpha = 255,                     // ###php-mode-test### ((indent 8))
    ) {}                                             // ###php-mode-test### ((indent 4))

    public function withAlpha(int $alpha): self      // ###php-mode-test### ((indent 4))
    {                                                // ###php-mode-test### ((indent 4))
        return clone($this, [                        // ###php-mode-test### ((indent 8))
            'alpha' => $alpha,                       // ###php-mode-test### ((indent 12))
        ]);                                          // ###php-mode-test### ((indent 8))
    }                                                // ###php-mode-test### ((indent 4))
}

$blue = new Color(79, 91, 147);
$transparentBlue = $blue->withAlpha(128);

// All on one line.
$copy = clone($blue, ['alpha' => 0]);                // ###php-mode-test### ((indent 0))

// Plain `clone' keeps working.
$plain = clone $blue;                                // ###php-mode-test### ((indent 0))

// The argument list broken across lines.
$other = clone(
    $blue,                                           // ###php-mode-test### ((indent 4))
    [                                                // ###php-mode-test### ((indent 4))
        'red' => 0,                                  // ###php-mode-test### ((indent 8))
        'green' => 0,                                // ###php-mode-test### ((indent 8))
    ],                                               // ###php-mode-test### ((indent 4))
);                                                   // ###php-mode-test### ((indent 0))
