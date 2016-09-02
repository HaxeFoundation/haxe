package flash.media;

extern class AVABRParameters {
	var maxBitsPerSecond : Int;
	var minBitsPerSecond : Int;
	var policy : String;
	var startBitsPerSecond : Int;
	function new(init_policy : String, init_startBitsPerSecond : UInt, init_minBitsPerSecond : UInt, init_maxBitsPerSecond : UInt) : Void;
	static var AGGRESSIVE(default,never) : String;
	static var CONSERVATIVE(default,never) : String;
	static var MODERATE(default,never) : String;
}
