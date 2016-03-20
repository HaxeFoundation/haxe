package flash.printing;

extern class PrintJob extends flash.events.EventDispatcher {
	var orientation(default,never) : PrintJobOrientation;
	var pageHeight(default,never) : Int;
	var pageWidth(default,never) : Int;
	var paperHeight(default,never) : Int;
	var paperWidth(default,never) : Int;
	function new() : Void;
	function addPage(sprite : flash.display.Sprite, ?printArea : flash.geom.Rectangle, ?options : PrintJobOptions, frameNum : Int = 0) : Void;
	function send() : Void;
	function start() : Bool;
	@:require(flash10_1) static var isSupported(default,never) : Bool;
}
