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
	static final CURRENT : String;
	static final MULTIPLE : String;
	static final NEXT : String;
	static final RASTER : String;
	static final SCREEN : String;
	static final STAGE : String;
}
