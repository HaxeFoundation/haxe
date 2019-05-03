package flash.printing;

extern class PrintJob extends flash.events.EventDispatcher {
	var orientation(get,never) : PrintJobOrientation;
	var pageHeight(get,never) : Int;
	var pageWidth(get,never) : Int;
	var paperHeight(get,never) : Int;
	var paperWidth(get,never) : Int;
	function new() : Void;
	function addPage(sprite : flash.display.Sprite, ?printArea : flash.geom.Rectangle, ?options : PrintJobOptions, frameNum : Int = 0) : Void;
	private function get_orientation() : PrintJobOrientation;
	private function get_pageHeight() : Int;
	private function get_pageWidth() : Int;
	private function get_paperHeight() : Int;
	private function get_paperWidth() : Int;
	function send() : Void;
	function start() : Bool;
	@:require(flash10_1) static var isSupported(get,never) : Bool;
	private static function get_isSupported() : Bool;
}
