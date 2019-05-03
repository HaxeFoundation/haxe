package flash.events;

extern class AVDictionaryDataEvent extends Event {
	var dictionary(get,never) : flash.utils.Dictionary;
	var time(get,never) : Float;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?init_dictionary : flash.utils.Dictionary, init_dataTime : Float = 0) : Void;
	private function get_dictionary() : flash.utils.Dictionary;
	private function get_time() : Float;
	static final AV_DICTIONARY_DATA : String;
}
