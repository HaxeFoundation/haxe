package flash.system;

@:final @:require(flash11_4) extern class MessageChannel extends flash.events.EventDispatcher {
	var messageAvailable(default,null) : Bool;
	var state(default,null) : MessageChannelState;
	function close() : Void;
	function receive(blockUntilReceived : Bool = false) : Dynamic;
	function send(arg : Dynamic, queueLimit : Int = -1) : Void;
}
