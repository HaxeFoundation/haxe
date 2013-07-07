package flash.net;

@:require(flash11) extern class NetMonitor extends flash.events.EventDispatcher {
	function new() : Void;
	function listStreams() : flash.Vector<NetStream>;
}
