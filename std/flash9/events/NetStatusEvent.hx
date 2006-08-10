package flash.events;

extern class NetStatusEvent extends flash.events.Event {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?info : Dynamic) : Void;
	var info : Dynamic;
	private var m_info : Dynamic;
	static var NET_STATUS : String;
}
