package php7.db;

import php7.RuntimeException;

@:native('PDOException')
extern class PDOException extends RuntimeException {
    var errorInfo : NativeArray;
}