package flash.events;

extern class DataEvent extends flash.events.TextEvent {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?data : String) : Void;
	var data : String;
	static var DATA : String;
}
