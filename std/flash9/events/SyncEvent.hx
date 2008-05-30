package flash.events;

extern class SyncEvent extends Event {
	var changeList : Array<Dynamic>;
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?changeList : Array<Dynamic>) : Void;
	static var SYNC : String;
}
