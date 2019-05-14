package flash.net;

@:require(flash10_1) extern final class NetGroupInfo {
	@:flash.property var objectReplicationReceiveBytesPerSecond(get,never) : Float;
	@:flash.property var objectReplicationSendBytesPerSecond(get,never) : Float;
	@:flash.property var postingReceiveControlBytesPerSecond(get,never) : Float;
	@:flash.property var postingReceiveDataBytesPerSecond(get,never) : Float;
	@:flash.property var postingSendControlBytesPerSecond(get,never) : Float;
	@:flash.property var postingSendDataBytesPerSecond(get,never) : Float;
	@:flash.property var routingReceiveBytesPerSecond(get,never) : Float;
	@:flash.property var routingSendBytesPerSecond(get,never) : Float;
	function new(postingSendDataBytesPerSecond : Float, postingSendControlBytesPerSecond : Float, postingReceiveDataBytesPerSecond : Float, postingReceiveControlBytesPerSecond : Float, routingSendBytesPerSecond : Float, routingReceiveBytesPerSecond : Float, objectReplicationSendBytesPerSecond : Float, objectReplicationReceiveBytesPerSecond : Float) : Void;
	private function get_objectReplicationReceiveBytesPerSecond() : Float;
	private function get_objectReplicationSendBytesPerSecond() : Float;
	private function get_postingReceiveControlBytesPerSecond() : Float;
	private function get_postingReceiveDataBytesPerSecond() : Float;
	private function get_postingSendControlBytesPerSecond() : Float;
	private function get_postingSendDataBytesPerSecond() : Float;
	private function get_routingReceiveBytesPerSecond() : Float;
	private function get_routingSendBytesPerSecond() : Float;
	function toString() : String;
}
