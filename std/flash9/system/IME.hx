package flash.system;

extern class IME extends flash.events.EventDispatcher {
	static var constructOK(null,default) : Void;
	static var conversionMode : IMEConversionMode;
	static var enabled : Bool;
	static function doConversion() : Void;
	static function setCompositionString(composition : String) : Void;
}
