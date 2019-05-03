package flash.system;

extern class IME extends flash.events.EventDispatcher {
	static var conversionMode(get,set) : IMEConversionMode;
	static var enabled(get,set) : Bool;
	@:require(flash10_1) static var isSupported(get,never) : Bool;
	@:require(flash10_1) static function compositionAbandoned() : Void;
	@:require(flash10_1) static function compositionSelectionChanged(start : Int, end : Int) : Void;
	static function doConversion() : Void;
	private static function get_conversionMode() : IMEConversionMode;
	private static function get_enabled() : Bool;
	private static function get_isSupported() : Bool;
	static function setCompositionString(composition : String) : Void;
	private static function set_conversionMode(value : IMEConversionMode) : IMEConversionMode;
	private static function set_enabled(value : Bool) : Bool;
}
