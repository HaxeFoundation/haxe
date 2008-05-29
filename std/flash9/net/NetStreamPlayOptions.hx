package flash.net;

extern class NetStreamPlayOptions extends flash.events.EventDispatcher {
	var oldStreamName : String;
	var len : Float;
	var start : Float;
	var streamName : String;
	var transition : NetStreamPlayTransitions;
	public function new() : Void;
}
