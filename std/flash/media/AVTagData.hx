package flash.media;

extern class AVTagData {
	@:flash.property var data(get,never) : String;
	@:flash.property var localTime(get,never) : Float;
	function new(init_data : String, init_localTime : Float) : Void;
	private function get_data() : String;
	private function get_localTime() : Float;
}
