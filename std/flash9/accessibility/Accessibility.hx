package flash.accessibility;

extern class Accessibility {
	function new() : Void;
	static var active(default,null) : Bool;
	static function sendEvent(source : flash.display.DisplayObject, childID : UInt, eventType : UInt, ?nonHTML : Bool) : Void;
	static function updateProperties() : Void;
}
