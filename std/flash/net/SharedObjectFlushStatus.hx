package flash.net;

extern class SharedObjectFlushStatus {
	function new() : Void;
	static var FLUSHED(default,never) : String;
	static var PENDING(default,never) : String;
}
