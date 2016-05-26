package flash.events;

@:require(flash10_2) extern class VideoEvent extends Event {
	var codecInfo(default,never) : String;
	var status(default,never) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?status : String) : Void;
	static var RENDER_STATE(default,never) : String;
	static var RENDER_STATUS_ACCELERATED(default,never) : String;
	static var RENDER_STATUS_SOFTWARE(default,never) : String;
	static var RENDER_STATUS_UNAVAILABLE(default,never) : String;
}
