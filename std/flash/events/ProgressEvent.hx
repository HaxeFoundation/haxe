package flash.events;

extern class ProgressEvent extends Event {
	var bytesLoaded : Float;
	var bytesTotal : Float;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, bytesLoaded : Float = 0, bytesTotal : Float = 0) : Void;
	static var PROGRESS(default,never) : String;
	static var SOCKET_DATA(default,never) : String;
}
