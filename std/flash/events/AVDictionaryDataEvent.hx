package flash.events;

extern class AVDictionaryDataEvent extends Event {
	@:flash.property var dictionary(get,never) : flash.utils.Dictionary;
	@:flash.property var time(get,never) : Float;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?init_dictionary : flash.utils.Dictionary, init_dataTime : Float = 0) : Void;
	private function get_dictionary() : flash.utils.Dictionary;
	private function get_time() : Float;
	static final AV_DICTIONARY_DATA : String;
}
