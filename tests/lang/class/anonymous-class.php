<?php

$class = new class () extends IteratorAggregate {
    // ###php-mode-test### ((indent 4))
};

is_object(1, new class () extends IteratorAggregate {
    // ###php-mode-test### ((indent 4))
});
