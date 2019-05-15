<?php

new stdClass;
@new stdClass;
echo @a.@b;
echo @a;
echo @$a;

@fopen('php://memory', 'rw');
