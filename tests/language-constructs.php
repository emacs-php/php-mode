<?php

/**
 * Test highlighting of language constructs and reserved keywords
 *
 * This test is based on http://php.net/manual/en/reserved.keywords.php and should
 * be updated from the manual when new PHP versions arrive. Built-in functions are
 * not treated differently by php-mode than regular function calls: that means
 * they are not highlighted. Only language constructs like print/die and reserved
 * keywords are, and those are tested here.
 */

// Start:
__halt_compiler();
abstract;
and;
array();
as;
break;
callable;
case;
catch;
class ClassName;
clone;
const;
continue;
declare;
default;
die();
do;
echo;
else;
elseif;
empty();
enddeclare;
endfor;
endforeach;
endif;
endswitch;
endwhile;
eval();
exit();
extends ClassName;
final;
finally;
for;
foreach;
function;
global;
goto;
if;
implements ClassName;
include;
include_once;
instanceof ClassName;
insteadof ClassName;
interface ClassName;
isset();
iterable;
list();
namespace ClassName;
new ClassName;
print;
private;
protected;
public;
require;
require_once;
return;
static;
switch;
throw;
trait ClassName;
try;
unset();
use ClassName;
var;
while;
xor;
yield;
yield from;
