package flash.events;

extern class EventPhase {
	function new() : Void;
	static var AT_TARGET : UInt;
	static var BUBBLING_PHASE : UInt;
	static var CAPTURING_PHASE : UInt;
}
