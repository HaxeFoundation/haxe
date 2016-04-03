package lua;

@:native("_G.os")
extern class Os {
	public static function clock() : Float;

	@:overload(   function     (format : String, time : Time) : DateType {})
	@:overload(   function     (format : String) : DateType {})
	public static function date() : DateType;

	public static function difftime(t2: Time, t1: Time) : Float;

	// TODO: multi-return
	public static function execute(?command:String) : Bool;

	public static function exit(code: Int) : Int;
	public static function getenv(varname : String) : String;
	public static function remove(filename : String) : Void;
	public static function rename(oldname : String, newname : String) : Void;
	public static function setlocale(locale : String, ?category : LocaleCategory ) : String;
	public static function time(?arg : TimeParam) : Time;
	public static function tmpname() : String;
}

/**
  A typedef that matches the date parameter time() will accept.
 **/
typedef TimeParam = {
	year   : Float,
	month  : Float,
	day    : Float,
	?hour  : Int,
	?min   : Int,
	?sec   : Int,
	?isdst : Bool
}

/**
  A typedef that describes the output of date().
 **/
typedef DateType = {
	hour  : Int,
	min   : Int,
	sec   : Int,
	isdst : Bool,
	year  : Int,
	month : Int,
	wday  : Int,
	yday  : Int,
	day   : Int,
}
