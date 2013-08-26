<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/68
 *
 * The 'catch' keywords should align with 'try' and not march
 * off to the right side of the screen like they do in the bug
 * report at the URL above.
 *
 */

/* CC Mode uses the indentation for 'block-close' for these 'catch'
 * statements.  However, that is assuming that the closing braces for
 * 'try' and each 'catch' align with the keywords themselves.  If they
 * do not then CC Mode gets confused and starts trying to indent
 * 'catch' blocks as if they were function definitions and other
 * strange things.
 *
 * The easiest way to deal with this is to make sure that the
 * `c-block-stmt-2-key' list contains the string "catch", which we
 * take care of in the definition of `php-block-stmt-2-key'.
 */

try {
        $mongo = new \Mongo($dsn, array("replicaSet" => 'mat_replica'));
} catch (\MongoConnectionException $e) {
        throw $e;
} catch (\Exception $e) {
        throw $e;
}

/* CC Mode uses the indentation syntax for 'topmost-intro' for the
 * 'catch' keywords in the remaining tests.
 */

try {
        $mongo = new \Mongo($dsn, array("replicaSet" => 'mat_replica'));
}
catch (\MongoConnectionException $e) {
        throw $e;
}
catch (\Exception $e) {
        throw $e;
}

try
{
        $mongo = new \Mongo($dsn, array("replicaSet" => 'mat_replica'));
}
catch (\MongoConnectionException $e)
{
        throw $e;
}
catch (\Exception $e)
{
        throw $e;
}
