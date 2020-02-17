package flash.events;

extern class IOErrorEvent extends ErrorEvent {
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?text : String, id : Int = 0) : Void;
	static final DISK_ERROR : String;
	static final IO_ERROR : String;
	static final NETWORK_ERROR : String;
	static final VERIFY_ERROR : String;
}
