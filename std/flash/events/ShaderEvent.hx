package flash.events;

extern class ShaderEvent extends Event {
	@:flash.property var bitmapData(get,set) : flash.display.BitmapData;
	@:flash.property var byteArray(get,set) : flash.utils.ByteArray;
	@:flash.property var vector(get,set) : flash.Vector<Float>;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?bitmap : flash.display.BitmapData, ?array : flash.utils.ByteArray, ?vector : flash.Vector<Float>) : Void;
	private function get_bitmapData() : flash.display.BitmapData;
	private function get_byteArray() : flash.utils.ByteArray;
	private function get_vector() : flash.Vector<Float>;
	private function set_bitmapData(value : flash.display.BitmapData) : flash.display.BitmapData;
	private function set_byteArray(value : flash.utils.ByteArray) : flash.utils.ByteArray;
	private function set_vector(value : flash.Vector<Float>) : flash.Vector<Float>;
	static final COMPLETE : String;
}
