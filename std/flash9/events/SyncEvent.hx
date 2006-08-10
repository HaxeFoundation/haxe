package flash.events;

extern class SyncEvent extends flash.events.Event {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?changeList : Array<Dynamic>) : Void;
	var changeList : Array<Dynamic>;
	private var m_changeList : Array<Dynamic>;
	static var SYNC : String;
}
