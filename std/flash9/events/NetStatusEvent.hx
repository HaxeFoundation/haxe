package flash.events;

extern class NetStatusEvent extends Event {
	var info : Dynamic;
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?info : Dynamic) : Void;
	static var NET_STATUS : String;
}
