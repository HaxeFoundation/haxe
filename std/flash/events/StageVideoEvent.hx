package flash.events;

@:require(flash10_2) extern class StageVideoEvent extends Event {
	final codecInfo : String;
	var colorSpace(default,never) : String;
	var status(default,never) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?status : String, ?colorSpace : String) : Void;
	static final RENDER_STATE : String;
	static final RENDER_STATUS_ACCELERATED : String;
	static final RENDER_STATUS_SOFTWARE : String;
	static final RENDER_STATUS_UNAVAILABLE : String;
}
