package flash.events;

extern class ProgressEvent extends Event {
	var bytesLoaded : UInt;
	var bytesTotal : UInt;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, bytesLoaded : UInt = 0, bytesTotal : UInt = 0) : Void;
	static var PROGRESS : String;
	static var SOCKET_DATA : String;
}
