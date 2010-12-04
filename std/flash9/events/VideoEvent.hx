package flash.events;

@:require(flash10_2) extern class VideoEvent extends Event {
	var status(default,null) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?status : String) : Void;
	static var RENDER_STATE : String;
	static var RENDER_STATUS_ACCELERATED : String;
	static var RENDER_STATUS_SOFTWARE : String;
	static var RENDER_STATUS_UNAVAILABLE : String;
}
