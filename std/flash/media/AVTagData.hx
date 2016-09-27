package flash.media;

extern class AVTagData {
	var data(default,never) : String;
	var localTime(default,never) : Float;
	function new(init_data : String, init_localTime : Float) : Void;
}
