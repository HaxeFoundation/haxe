package flash.system;

#use_ime

extern class IME {

	static var _ALPHANUMERIC_FULL : String;
	static var _ALPHANUMERIC_HALF : String;
	static var _CHINESE : String;
	static var _JAPANESE_HIRAGANA : String;
	static var _JAPANESE_KATAKANA_FULL : String;
	static var _JAPANESE_KATAKANA_HALF : String;
	static var _KOREAN : String;
	static var _UNKNOWN : String;

	static function getEnabled() : Bool;
	static function setEnabled(enabled:Bool) : Bool;
	static function getConversionMode() : String;
	static function setConversionMode(mode:String) : Bool;
	static function setCompositionString (composition:String) : Bool;
	static function doConversion() : Bool;
	static function addListener(listener:Dynamic) : Void;
	static function removeListener(listener:Dynamic) : Bool;
}

#end
