package flash.automation;

@:require(flash10_1) extern class StageCapture extends flash.events.EventDispatcher {
	@:flash.property var capturePTS(get,set) : Float;
	@:flash.property var captureSource(get,set) : String;
	@:flash.property var clipRect(get,set) : flash.geom.Rectangle;
	@:flash.property var fileNameBase(get,set) : String;
	function new() : Void;
	function cancel() : Void;
	function capture(type : String) : Void;
	function captureBitmapData() : flash.display.BitmapData;
	private function get_capturePTS() : Float;
	private function get_captureSource() : String;
	private function get_clipRect() : flash.geom.Rectangle;
	private function get_fileNameBase() : String;
	private function set_capturePTS(value : Float) : Float;
	private function set_captureSource(value : String) : String;
	private function set_clipRect(value : flash.geom.Rectangle) : flash.geom.Rectangle;
	private function set_fileNameBase(value : String) : String;
	static final CURRENT : String;
	static final MULTIPLE : String;
	static final NEXT : String;
	static final RASTER : String;
	static final SCREEN : String;
	static final STAGE : String;
}
