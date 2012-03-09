package flash.events;

@:require(flash10_2) extern class StageVideoEvent extends Event {
	var codecInfo : String;
	var colorSpace(default,null) : String;
	var status(default,null) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?status : String, ?colorSpace : String) : Void;
	static var RENDER_STATE : String;
	static var RENDER_STATUS_ACCELERATED : String;
	static var RENDER_STATUS_SOFTWARE : String;
	static var RENDER_STATUS_UNAVAILABLE : String;
}
