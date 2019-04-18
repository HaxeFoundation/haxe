package flash.events;

extern class DataEvent extends TextEvent {
	var data : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?data : String) : Void;
	static final DATA : String;
	static final UPLOAD_COMPLETE_DATA : String;
}
