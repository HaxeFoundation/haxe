package flash.events;

extern class DataEvent extends TextEvent {
	var data : String;
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?data : String) : Void;
	static var DATA : String;
	static var UPLOAD_COMPLETE_DATA : String;
}
