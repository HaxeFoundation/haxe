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
}