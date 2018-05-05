package flash.events;

extern class VideoTextureEvent extends Event {
	var codecInfo(default,never) : String;
	var colorSpace(default,never) : String;
	var status(default,never) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?status : String, ?colorSpace : String) : Void;
	static var RENDER_STATE(default,never) : String;
}
