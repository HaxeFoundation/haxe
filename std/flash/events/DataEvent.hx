package flash.events;

extern class DataEvent extends TextEvent {
	var data : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?data : String) : Void;
	static var DATA : String;
	static var UPLOAD_COMPLETE_DATA : String;
}
