package flash.printing;

extern class PrintJob extends flash.events.EventDispatcher {
	var orientation(default,null) : PrintJobOrientation;
	var pageHeight(default,null) : Int;
	var pageWidth(default,null) : Int;
	var paperHeight(default,null) : Int;
	var paperWidth(default,null) : Int;
	function new() : Void;
	function addPage(sprite : flash.display.Sprite, ?printArea : flash.geom.Rectangle, ?options : PrintJobOptions, frameNum : Int = 0) : Void;
	function send() : Void;
	function start() : Bool;
	@:require(flash10_1) static var isSupported(default,null) : Bool;
}
