package flash.media;

extern class AVABRProfileInfo {
	var bitsPerSecond(default,never) : Int;
	var height(default,never) : Int;
	var width(default,never) : Int;
	function new(init_bitsPerSecond : Int, init_width : Int, init_height : Int) : Void;
}
