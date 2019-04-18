package flash.events;

extern class NetDataEvent extends Event {
	var info(default,never) : Dynamic;
	var timestamp(default,never) : Float;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, timestamp : Float = 0, ?info : Dynamic) : Void;
	static final MEDIA_TYPE_DATA : String;
}
