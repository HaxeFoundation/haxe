package flash.automation;

@:require(flash10_1) extern class StageCapture extends flash.events.EventDispatcher {
	var clipRect : flash.geom.Rectangle;
	var fileNameBase : String;
	function new() : Void;
	function cancel() : Void;
	function capture(type : String) : Void;
	static var CURRENT : String;
	static var MULTIPLE : String;
	static var NEXT : String;
}
