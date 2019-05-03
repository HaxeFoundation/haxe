package flash.display;

extern class Bitmap extends DisplayObject {
	var bitmapData(get,set) : BitmapData;
	var pixelSnapping(get,set) : PixelSnapping;
	var smoothing(get,set) : Bool;
	function new(?bitmapData : BitmapData, ?pixelSnapping : PixelSnapping, smoothing : Bool = false) : Void;
	private function get_bitmapData() : BitmapData;
	private function get_pixelSnapping() : PixelSnapping;
	private function get_smoothing() : Bool;
	private function set_bitmapData(value : BitmapData) : BitmapData;
	private function set_pixelSnapping(value : PixelSnapping) : PixelSnapping;
	private function set_smoothing(value : Bool) : Bool;
}
