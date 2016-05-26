package flash.automation;

@:require(flash10_1) extern class StageCapture extends flash.events.EventDispatcher {
	var capturePTS : Float;
	var captureSource : String;
	var clipRect : flash.geom.Rectangle;
	var fileNameBase : String;
	function new() : Void;
	function cancel() : Void;
	function capture(type : String) : Void;
	function captureBitmapData() : flash.display.BitmapData;
	static var CURRENT(default,never) : String;
	static var MULTIPLE(default,never) : String;
	static var NEXT(default,never) : String;
	static var RASTER(default,never) : String;
	static var SCREEN(default,never) : String;
	static var STAGE(default,never) : String;
}
