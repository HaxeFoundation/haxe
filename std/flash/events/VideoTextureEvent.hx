package flash.events;

extern class VideoTextureEvent extends Event {
	final codecInfo : String;
	var colorSpace(default,never) : String;
	var status(default,never) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?status : String, ?colorSpace : String) : Void;
	static final RENDER_STATE : String;
}
