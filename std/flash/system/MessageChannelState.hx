package flash.system;

extern class MessageChannelState {
	function new() : Void;
	static var CLOSED(default,never) : String;
	static var CLOSING(default,never) : String;
	static var OPEN(default,never) : String;
}
