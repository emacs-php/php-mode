<?php

/**
 * Test for type annotation in doc comment
 *
 * The specification is defined in PSR-5 (draft) Appendix A. Types.
 * This implementation contains unstandardized specification.
 *
 * @see https://github.com/php-fig/fig-standards/blob/master/proposed/phpdoc.md#appendix-a-types
 */

// Test for premitive type (int)

/** @return int   A integer value */
/** @return ?int  A nullable integer value */
/** @return int[] A list of integer values */


// Test for class type (DateTime)

/** @return DateTime   A DateTime object value */
/** @return ?DateTime  A nullable DateTime object value */
/** @return DateTime[] A list of DateTime object values */


// Test for class type (stdClass)

/** @return stdClass   A stdClass object value */
/** @return ?stdClass  A nullable stdClass object value */
/** @return stdClass[] A list of stdClass object values */

// Test for class type (\App\User)

/** @return \App\User   A \App\User object value */
/** @return ?\App\User  A nullable \App\User object value */
/** @return \App\User[] A list of \App\User object values */

// Test for multiple types

/** @return int|string    Multiple types by int and string */
/** @return int[]|string  Multiple types by list of int and string */
/** @return int|stdClass    Multiple types by int and stdClass */
/** @return int|\App\User    Multiple types by int and \App\User */
/** @return DateTime|int    Multiple types by DateTime and int */
/** @return \App\User|int    Multiple types by \App\User and int */

// Test for $this

/** @return $this this is special variable */
/** @return $that that is NOT special variable */
