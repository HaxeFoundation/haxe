package flash.media;

extern class AVABRProfileInfo {
	@:flash.property var bitsPerSecond(get,never) : Int;
	@:flash.property var height(get,never) : Int;
	@:flash.property var width(get,never) : Int;
	function new(init_bitsPerSecond : Int, init_width : Int, init_height : Int) : Void;
	private function get_bitsPerSecond() : Int;
	private function get_height() : Int;
	private function get_width() : Int;
}
