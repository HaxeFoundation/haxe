package flash.system;

extern class IME extends flash.events.EventDispatcher {
	function new() : Void;
	static var constructOK(null,default) : Void;
	static var conversionMode : String;
	static function doConversion() : Void;
	static var enabled : Bool;
	static function setCompositionString(composition : String) : Void;
}
