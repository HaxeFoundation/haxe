package php7;

/**
    This class contains externs for native PHP constants defined in global namespace.
    For native PHP functions in global namespace see `php7.Global`.
**/
@:phpGlobal
extern class Const {
    /**
        @see http://php.net/manual/en/errorfunc.constants.php
    **/
    static var E_ERROR : Int;
    static var E_WARNING : Int;
    static var E_PARSE : Int;
    static var E_NOTICE : Int;
    static var E_CORE_ERROR : Int;
    static var E_CORE_WARNING : Int;
    static var E_COMPILE_ERROR : Int;
    static var E_COMPILE_WARNING : Int;
    static var E_USER_ERROR : Int;
    static var E_USER_WARNING : Int;
    static var E_USER_NOTICE : Int;
    static var E_STRICT : Int;
    static var E_RECOVERABLE_ERROR : Int;
    static var E_DEPRECATED : Int;
    static var E_USER_DEPRECATED : Int;
    static var E_ALL : Int;
}