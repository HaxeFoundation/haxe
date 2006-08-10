package flash.display;

extern class Bitmap extends flash.display.DisplayObject {
	function new(?bitmapData : flash.display.BitmapData, ?pixelSnapping : String, ?smoothing : Bool) : Void;
	var bitmapData : flash.display.BitmapData;
	var pixelSnapping : String;
	var smoothing : Bool;
}
