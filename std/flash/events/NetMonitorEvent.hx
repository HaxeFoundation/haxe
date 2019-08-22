package flash.events;

extern class NetMonitorEvent extends Event {
	@:flash.property var netStream(get,never) : flash.net.NetStream;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?netStream : flash.net.NetStream) : Void;
	private function get_netStream() : flash.net.NetStream;
	static final NET_STREAM_CREATE : String;
}
