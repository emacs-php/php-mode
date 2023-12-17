<?php

$someObject->someFunction("some", "parameter") // ###php-mode-test### ((indent 0))
    ->someOtherFunc(23, 42) // ###php-mode-test### ((indent 4))
    ->andAThirdFunction(); // ###php-mode-test### ((indent 4))

$result = DateTime::createFromFormat('Y-m-d', '2112-09-03') // ###php-mode-test### ((indent 0))
    ->someFunction(); // ###php-mode-test### ((indent 4))

$pages = $dbOld->createQueryBuilder() // ###php-mode-test### ((indent 0))
    ->select('*'); // ###php-mode-test### ((indent 4))
