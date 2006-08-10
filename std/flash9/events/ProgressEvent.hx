package flash.events;

extern class ProgressEvent extends flash.events.Event {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?bytesLoaded : UInt, ?bytesTotal : UInt) : Void;
	var bytesLoaded : UInt;
	var bytesTotal : UInt;
	private var m_bytesLoaded : UInt;
	private var m_bytesTotal : UInt;
	static var PROGRESS : String;
	static var SOCKET_DATA : String;
}
