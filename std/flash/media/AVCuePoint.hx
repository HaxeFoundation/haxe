package flash.media;

extern class AVCuePoint {
	var dictionary(default,null) : flash.utils.Dictionary;
	var localTime(default,null) : Float;
	function new(init_dictionary : flash.utils.Dictionary, init_localTime : Float) : Void;
}
