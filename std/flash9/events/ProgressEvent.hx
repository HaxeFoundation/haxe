package flash.events;

extern class ProgressEvent extends Event {
	var bytesLoaded : UInt;
	var bytesTotal : UInt;
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?bytesLoaded : UInt, ?bytesTotal : UInt) : Void;
	static var PROGRESS : String;
	static var SOCKET_DATA : String;
}
