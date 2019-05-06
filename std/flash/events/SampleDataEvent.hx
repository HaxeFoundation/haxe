package flash.events;

extern class SampleDataEvent extends Event {
	@:flash.property var data(get,set) : flash.utils.ByteArray;
	@:flash.property var position(get,set) : Float;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, theposition : Float = 0, ?thedata : flash.utils.ByteArray) : Void;
	private function get_data() : flash.utils.ByteArray;
	private function get_position() : Float;
	private function set_data(value : flash.utils.ByteArray) : flash.utils.ByteArray;
	private function set_position(value : Float) : Float;
	static final SAMPLE_DATA : String;
}
