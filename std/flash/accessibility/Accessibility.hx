package flash.accessibility;

extern class Accessibility {
	static var active(default,never) : Bool;
	static function sendEvent(source : flash.display.DisplayObject, childID : UInt, eventType : UInt, nonHTML : Bool = false) : Void;
	static function updateProperties() : Void;
}
