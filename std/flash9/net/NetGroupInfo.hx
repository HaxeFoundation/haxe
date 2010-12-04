package flash.net;

@:require(flash10_1) @:final extern class NetGroupInfo {
	var objectReplicationReceiveBytesPerSecond(default,null) : Float;
	var objectReplicationSendBytesPerSecond(default,null) : Float;
	var postingReceiveControlBytesPerSecond(default,null) : Float;
	var postingReceiveDataBytesPerSecond(default,null) : Float;
	var postingSendControlBytesPerSecond(default,null) : Float;
	var postingSendDataBytesPerSecond(default,null) : Float;
	var routingReceiveBytesPerSecond(default,null) : Float;
	var routingSendBytesPerSecond(default,null) : Float;
	function new(postingSendDataBytesPerSecond : Float, postingSendControlBytesPerSecond : Float, postingReceiveDataBytesPerSecond : Float, postingReceiveControlBytesPerSecond : Float, routingSendBytesPerSecond : Float, routingReceiveBytesPerSecond : Float, objectReplicationSendBytesPerSecond : Float, objectReplicationReceiveBytesPerSecond : Float) : Void;
	function toString() : String;
}
