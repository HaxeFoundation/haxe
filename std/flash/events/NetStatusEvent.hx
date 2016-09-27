package flash.events;

extern class NetStatusEvent extends Event {
	var info : Dynamic;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?info : Dynamic) : Void;
	static var NET_STATUS(default,never) : String;
}
