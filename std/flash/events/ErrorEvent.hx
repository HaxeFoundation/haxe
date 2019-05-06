package flash.events;

extern class ErrorEvent extends TextEvent {
	@:flash.property @:require(flash10_1) var errorID(get,never) : Int;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?text : String, id : Int = 0) : Void;
	private function get_errorID() : Int;
	static final ERROR : String;
}
