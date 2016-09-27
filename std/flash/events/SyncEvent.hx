package flash.events;

extern class SyncEvent extends Event {
	var changeList : Array<Dynamic>;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?changeList : Array<Dynamic>) : Void;
	static var SYNC(default,never) : String;
}
