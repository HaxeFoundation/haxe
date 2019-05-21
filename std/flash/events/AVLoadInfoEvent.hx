package flash.events;

extern class AVLoadInfoEvent extends Event {
	@:flash.property var loadInfo(get,never) : flash.utils.Dictionary;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?inLoadInfo : flash.utils.Dictionary) : Void;
	private function get_loadInfo() : flash.utils.Dictionary;
	static final AV_LOAD_INFO : Dynamic;
}
