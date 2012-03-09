package flash.events;

extern class NetDataEvent extends Event {
	var info(default,null) : Dynamic;
	var timestamp(default,null) : Float;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, timestamp : Float = 0, ?info : Dynamic) : Void;
	static var MEDIA_TYPE_DATA : String;
}
