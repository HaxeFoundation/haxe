package flash.events;

extern class VideoTextureEvent extends Event {
	var codecInfo : String;
	var colorSpace(default,null) : String;
	var status(default,null) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?status : String, ?colorSpace : String) : Void;
	static var RENDER_STATE : String;
}
