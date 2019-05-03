package flash.filters;

extern final class DisplacementMapFilter extends BitmapFilter {
	var alpha(get,set) : Float;
	var color(get,set) : UInt;
	var componentX(get,set) : UInt;
	var componentY(get,set) : UInt;
	var mapBitmap(get,set) : flash.display.BitmapData;
	var mapPoint(get,set) : flash.geom.Point;
	var mode(get,set) : DisplacementMapFilterMode;
	var scaleX(get,set) : Float;
	var scaleY(get,set) : Float;
	function new(?mapBitmap : flash.display.BitmapData, ?mapPoint : flash.geom.Point, componentX : UInt = 0, componentY : UInt = 0, scaleX : Float = 0, scaleY : Float = 0, ?mode : DisplacementMapFilterMode, color : UInt = 0, alpha : Float = 0) : Void;
	private function get_alpha() : Float;
	private function get_color() : UInt;
	private function get_componentX() : UInt;
	private function get_componentY() : UInt;
	private function get_mapBitmap() : flash.display.BitmapData;
	private function get_mapPoint() : flash.geom.Point;
	private function get_mode() : DisplacementMapFilterMode;
	private function get_scaleX() : Float;
	private function get_scaleY() : Float;
	private function set_alpha(value : Float) : Float;
	private function set_color(value : UInt) : UInt;
	private function set_componentX(value : UInt) : UInt;
	private function set_componentY(value : UInt) : UInt;
	private function set_mapBitmap(value : flash.display.BitmapData) : flash.display.BitmapData;
	private function set_mapPoint(value : flash.geom.Point) : flash.geom.Point;
	private function set_mode(value : DisplacementMapFilterMode) : DisplacementMapFilterMode;
	private function set_scaleX(value : Float) : Float;
	private function set_scaleY(value : Float) : Float;
}
