package flash.ui;

@:require(flash10_2) extern final class MouseCursorData {
	@:flash.property var data(get,set) : flash.Vector<flash.display.BitmapData>;
	@:flash.property var frameRate(get,set) : Float;
	@:flash.property var hotSpot(get,set) : flash.geom.Point;
	function new() : Void;
	private function get_data() : flash.Vector<flash.display.BitmapData>;
	private function get_frameRate() : Float;
	private function get_hotSpot() : flash.geom.Point;
	private function set_data(value : flash.Vector<flash.display.BitmapData>) : flash.Vector<flash.display.BitmapData>;
	private function set_frameRate(value : Float) : Float;
	private function set_hotSpot(value : flash.geom.Point) : flash.geom.Point;
}
