package flash.system;

@:require(flash11_4) extern final class MessageChannel extends flash.events.EventDispatcher {
	@:flash.property var messageAvailable(get,never) : Bool;
	@:flash.property var state(get,never) : MessageChannelState;
	function close() : Void;
	private function get_messageAvailable() : Bool;
	private function get_state() : MessageChannelState;
	function receive(blockUntilReceived : Bool = false) : Dynamic;
	function send(arg : Dynamic, queueLimit : Int = -1) : Void;
}
