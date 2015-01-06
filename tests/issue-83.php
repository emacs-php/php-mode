<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/83
 *
 * All static methods should appear on imenu whether 'static' keyword
 * is placed before or after visibility.
 *
 */

class Test
{
	static public function staticBeforeVisibility()
	{
		// Do nothing
	}

	public static function staticAfterVisibility()
	{
		// Do nothing
	}
}