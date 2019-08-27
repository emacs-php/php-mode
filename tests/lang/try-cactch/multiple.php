<?php

try {
} catch (ErrorException | PDOException $e) {
} catch (Foo | Bar | Buz | Fuga $e) {
} catch (Throwable $e) {
}
