package flash.media;

extern class AVCuePoint {
	var dictionary(default,never) : flash.utils.Dictionary;
	var localTime(default,never) : Float;
	function new(init_dictionary : flash.utils.Dictionary, init_localTime : Float) : Void;
}
