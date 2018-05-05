package flash.events;

extern class NetDataEvent extends Event {
	var info(default,never) : Dynamic;
	var timestamp(default,never) : Float;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, timestamp : Float = 0, ?info : Dynamic) : Void;
	static var MEDIA_TYPE_DATA(default,never) : String;
}
