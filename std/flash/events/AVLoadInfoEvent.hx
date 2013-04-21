package flash.events;

extern class AVLoadInfoEvent extends Event {
	var loadInfo(default,null) : flash.utils.Dictionary;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?inLoadInfo : flash.utils.Dictionary) : Void;
	static var AV_LOAD_INFO : Dynamic;
}
