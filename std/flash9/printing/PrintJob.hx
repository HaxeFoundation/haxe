package flash.printing;

extern class PrintJob extends flash.events.EventDispatcher {
	function new() : Void;
	function addPage(sprite : flash.display.Sprite, ?printArea : flash.geom.Rectangle, ?options : flash.printing.PrintJobOptions, ?frameNum : Int) : Void;
	var orientation(default,null) : String;
	var pageHeight(default,null) : Int;
	var pageWidth(default,null) : Int;
	var paperHeight(default,null) : Int;
	var paperWidth(default,null) : Int;
	function send() : Void;
	function start() : Bool;
	private function _invoke(index : Dynamic, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : Void;
	private function invoke(index : UInt, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : Void;
	private function toClassicRectangle(printArea : flash.geom.Rectangle) : Void;
	private static var kAddPage : UInt;
	private static var kGetOrientation : UInt;
	private static var kGetPageHeight : UInt;
	private static var kGetPageWidth : UInt;
	private static var kGetPaperHeight : UInt;
	private static var kGetPaperWidth : UInt;
	private static var kSend : UInt;
	private static var kStart : UInt;
}
