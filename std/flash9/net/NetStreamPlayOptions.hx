package flash.net;

extern class NetStreamPlayOptions extends flash.events.EventDispatcher, implements Dynamic {
	var len : Float;
	var oldStreamName : String;
	var start : Float;
	var streamName : String;
	var transition : NetStreamPlayTransitions;
	function new() : Void;
}
