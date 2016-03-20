package flash.system;

@:final @:require(flash11_4) extern class MessageChannel extends flash.events.EventDispatcher {
	var messageAvailable(default,never) : Bool;
	var state(default,never) : MessageChannelState;
	function close() : Void;
	function receive(blockUntilReceived : Bool = false) : Dynamic;
	function send(arg : Dynamic, queueLimit : Int = -1) : Void;
}
