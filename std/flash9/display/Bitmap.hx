package flash.display;

extern class Bitmap extends flash.display.DisplayObject {
	function new(?bitmapData : flash.display.BitmapData, ?pixelSnapping : PixelSnapping, ?smoothing : Bool) : Void;
	var bitmapData : flash.display.BitmapData;
	var pixelSnapping : PixelSnapping;
	var smoothing : Bool;
}
