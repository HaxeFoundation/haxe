package php.db;

import php.RuntimeException;

@:native('PDOException')
extern class PDOException extends RuntimeException {
    var errorInfo : NativeArray;
}