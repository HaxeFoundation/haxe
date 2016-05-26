package flash.system;

extern class IME extends flash.events.EventDispatcher {
	static var conversionMode : IMEConversionMode;
	static var enabled : Bool;
	@:require(flash10_1) static var isSupported(default,never) : Bool;
	@:require(flash10_1) static function compositionAbandoned() : Void;
	@:require(flash10_1) static function compositionSelectionChanged(start : Int, end : Int) : Void;
	static function doConversion() : Void;
	static function setCompositionString(composition : String) : Void;
}
