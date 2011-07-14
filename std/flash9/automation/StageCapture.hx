package flash.automation;

@:require(flash10_1) extern class StageCapture extends flash.events.EventDispatcher {
	var captureSource : String;
	var clipRect : flash.geom.Rectangle;
	var fileNameBase : String;
	function new() : Void;
	function cancel() : Void;
	function capture(type : String) : Void;
	function captureBitmapData() : flash.display.BitmapData;
	static var CURRENT : String;
	static var MULTIPLE : String;
	static var NEXT : String;
	static var RASTER : String;
	static var SCREEN : String;
	static var STAGE : String;
}
