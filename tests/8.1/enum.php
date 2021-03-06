<?php

enum Size
{
    case Small;
    case Medium;
    case Large;

    public static function fromLength(int $cm)
    {
        return match(true) {
            $cm < 50 => static::Small,
            $cm < 100 => static::Medium,
            default => static::Large,
        };
    }
}
