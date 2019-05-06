package flash.events;

extern class SyncEvent extends Event {
	@:flash.property var changeList(get,set) : Array<Dynamic>;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?changeList : Array<Dynamic>) : Void;
	private function get_changeList() : Array<Dynamic>;
	private function set_changeList(value : Array<Dynamic>) : Array<Dynamic>;
	static final SYNC : String;
}
