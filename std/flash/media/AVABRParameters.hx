package flash.media;

extern class AVABRParameters {
	@:flash.property var maxBitsPerSecond(get,set) : Int;
	@:flash.property var minBitsPerSecond(get,set) : Int;
	@:flash.property var policy(get,set) : String;
	@:flash.property var startBitsPerSecond(get,set) : Int;
	function new(init_policy : String, init_startBitsPerSecond : UInt, init_minBitsPerSecond : UInt, init_maxBitsPerSecond : UInt) : Void;
	private function get_maxBitsPerSecond() : Int;
	private function get_minBitsPerSecond() : Int;
	private function get_policy() : String;
	private function get_startBitsPerSecond() : Int;
	private function set_maxBitsPerSecond(value : Int) : Int;
	private function set_minBitsPerSecond(value : Int) : Int;
	private function set_policy(value : String) : String;
	private function set_startBitsPerSecond(value : Int) : Int;
	static final AGGRESSIVE : String;
	static final CONSERVATIVE : String;
	static final MODERATE : String;
}
