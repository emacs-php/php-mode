<?php

namespace SlowNamespace\More\Levels;

use Carbon\Carbon;
use Illuminate\Support\Collection;

/**
 * @author Charlie McMackin
 * @see https://github.com/ejmr/php-mode/issues/333
 */
class Hello
{
    /**
     * @var ThirdClass;
     */
    protected $fors;

    /**
     * @var FourthClassRepository;
     */
    protected $psr2s;

    public function __construct(
        ThirdClass $for,
        FourthClass $psr2
    ) {
    }
}
