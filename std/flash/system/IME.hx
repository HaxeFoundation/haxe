package flash.system;

extern class IME {

	static var ALPHANUMERIC_FULL : String;
	static var ALPHANUMERIC_HALF : String;
	static var CHINESE : String;
	static var JAPANESE_HIRAGANA : String;
	static var JAPANESE_KATAKANA_FULL : String;
	static var JAPANESE_KATAKANA_HALF : String;
	static var KOREAN : String;
	static var UNKNOWN : String;

	static function getEnabled() : Bool;
	static function setEnabled(enabled:Bool) : Bool;
	static function getConversionMode() : String;
	static function setConversionMode(mode:String) : Bool;
	static function setCompositionString (composition:String) : Bool;
	static function doConversion() : Bool;
	static function addListener(listener:Dynamic) : Void;
	static function removeListener(listener:Dynamic) : Bool;

	private static function __init__() : Void untyped {
		flash.system.IME = _global.System.IME;
	}

}
