package flash;

#if flash_lite

/** flash lite only **/
extern class ExtendedKey {

	static var SOFT1 : String;
	static var SOFT2 : String;
	static var SOFT3 : String;
	static var SOFT4 : String;
	static var SOFT5 : String;
	static var SOFT6 : String;
	static var SOFT7 : String;
	static var SOFT8 : String;
	static var SOFT9 : String;
	static var SOFT10 : String;
	static var SOFT11 : String;
	static var SOFT12 : String;

	private static function __init__() : Void untyped {
		flash.ExtendedKey = _global["ExtendedKey"];
	}

}

#else
	#error
#end
