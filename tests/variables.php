<?php

// Test highlighting of variables with the variable-name-face
$_;
$regularVariable;
$$variableVariable;
$漢字;
$snake_case;
$abc123xyz;
$def_456_ghi;
${'var'};
${"var"};
${$var};
${'v' . 'ar'};
$a{1};
$a{'a'};
MyClass::$staticVariable;
$object->memberVariable;
$object->funCall();
