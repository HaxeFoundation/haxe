package flash.events;

extern class ErrorEvent extends TextEvent {
	@:require(flash10_1) var errorID(default,never) : Int;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?text : String, id : Int = 0) : Void;
	static var ERROR(default,never) : String;
}
