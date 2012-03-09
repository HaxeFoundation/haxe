package flash.accessibility;

extern class Accessibility {
	static var active(default,null) : Bool;
	static function sendEvent(source : flash.display.DisplayObject, childID : UInt, eventType : UInt, nonHTML : Bool = false) : Void;
	static function updateProperties() : Void;
}
