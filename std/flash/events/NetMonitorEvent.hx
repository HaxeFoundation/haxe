package flash.events;

extern class NetMonitorEvent extends Event {
	var netStream(default,never) : flash.net.NetStream;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?netStream : flash.net.NetStream) : Void;
	static var NET_STREAM_CREATE(default,never) : String;
}
