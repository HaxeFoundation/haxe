package flash.media;

extern class AVCuePoint {
	var dictionary(get,never) : flash.utils.Dictionary;
	var localTime(get,never) : Float;
	function new(init_dictionary : flash.utils.Dictionary, init_localTime : Float) : Void;
	private function get_dictionary() : flash.utils.Dictionary;
	private function get_localTime() : Float;
}
