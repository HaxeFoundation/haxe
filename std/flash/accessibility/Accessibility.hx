package flash.accessibility;

extern class Accessibility {
	@:flash.property static var active(get,never) : Bool;
	private static function get_active() : Bool;
	static function sendEvent(source : flash.display.DisplayObject, childID : UInt, eventType : UInt, nonHTML : Bool = false) : Void;
	static function updateProperties() : Void;
}
