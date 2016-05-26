package flash.events;

extern class AVDictionaryDataEvent extends Event {
	var dictionary(default,never) : flash.utils.Dictionary;
	var time(default,never) : Float;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, ?init_dictionary : flash.utils.Dictionary, init_dataTime : Float = 0) : Void;
	static var AV_DICTIONARY_DATA(default,never) : String;
}
