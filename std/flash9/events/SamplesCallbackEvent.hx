package flash.events;

extern class SamplesCallbackEvent extends Event {
	var position : Float;
	function new( ?type : String, ?bubbles : Bool, ?cancelable : Bool, ?position : Float ) : Void;
}
