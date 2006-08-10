package flash.events;

extern class IOErrorEvent extends flash.events.ErrorEvent {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?text : String) : Void;
	static var DISK_ERROR : String;
	static var IO_ERROR : String;
	static var NETWORK_ERROR : String;
	static var VERIFY_ERROR : String;
}
