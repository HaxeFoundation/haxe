package php7;

/**
	This class contains externs for native PHP constants defined in global namespace.
	For native PHP functions in global namespace see `php7.Global`.
**/
@:phpGlobal
extern class Const {
	/**
		If this constant is defined and equals `true` then Haxe will not set error handler automatically.
	**/
	static var HAXE_CUSTOM_ERROR_HANDLER : Bool;
	/**
		@see http://php.net/manual/en/reserved.constants.php
	**/
	static var PHP_INT_MAX : Int;
	static var PHP_INT_MIN : Int;
	static var PHP_INT_SIZE : Int;
	/**
		@see http://php.net/manual/en/language.constants.predefined.php
	**/
	static var __LINE__ : Int;
	static var __FILE__ : String;
	static var __DIR__ : String;
	static var __FUNCTION__ : String;
	static var __CLASS__ : String;
	static var __TRAIT__ : String;
	static var __METHOD__ : String;
	static var __NAMESPACE__ : String;
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
	/**
		@see http://php.net/manual/en/function.count.php
	**/
	static var COUNT_NORMAL : Int;
	static var COUNT_RECURSIVE : Int;
	/**
		@see http://php.net/manual/en/function.array-filter.php
	**/
	static var ARRAY_FILTER_USE_KEY : Int;
	static var ARRAY_FILTER_USE_BOTH : Int;
	/**
		@see http://php.net/manual/en/function.debug-backtrace.php
	**/
	static var DEBUG_BACKTRACE_PROVIDE_OBJECT : Int;
	static var DEBUG_BACKTRACE_IGNORE_ARGS : Int;
	/**
		@see http://php.net/manual/en/math.constants.php
	**/
	static var M_PI : Float;
	static var M_E : Float;
	static var M_LOG2E : Float;
	static var M_LOG10E : Float;
	static var M_LN2 : Float;
	static var M_LN10 : Float;
	static var M_PI_2 : Float;
	static var M_PI_4 : Float;
	static var M_1_PI : Float;
	static var M_2_PI : Float;
	static var M_SQRTPI : Float;
	static var M_2_SQRTPI : Float;
	static var M_SQRT2 : Float;
	static var M_SQRT3 : Float;
	static var M_SQRT1_2 : Float;
	static var M_LNPI : Float;
	static var M_EULER : Float;
	static var PHP_ROUND_HALF_UP : Int;
	static var PHP_ROUND_HALF_DOWN : Int;
	static var PHP_ROUND_HALF_EVEN : Int;
	static var PHP_ROUND_HALF_ODD : Int;
	static var NAN : Float;
	static var INF : Float;
}