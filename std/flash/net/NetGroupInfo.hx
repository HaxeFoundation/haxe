package flash.net;

@:final @:require(flash10_1) extern class NetGroupInfo {
	var objectReplicationReceiveBytesPerSecond(default,never) : Float;
	var objectReplicationSendBytesPerSecond(default,never) : Float;
	var postingReceiveControlBytesPerSecond(default,never) : Float;
	var postingReceiveDataBytesPerSecond(default,never) : Float;
	var postingSendControlBytesPerSecond(default,never) : Float;
	var postingSendDataBytesPerSecond(default,never) : Float;
	var routingReceiveBytesPerSecond(default,never) : Float;
	var routingSendBytesPerSecond(default,never) : Float;
	function new(postingSendDataBytesPerSecond : Float, postingSendControlBytesPerSecond : Float, postingReceiveDataBytesPerSecond : Float, postingReceiveControlBytesPerSecond : Float, routingSendBytesPerSecond : Float, routingReceiveBytesPerSecond : Float, objectReplicationSendBytesPerSecond : Float, objectReplicationReceiveBytesPerSecond : Float) : Void;
	function toString() : String;
}
