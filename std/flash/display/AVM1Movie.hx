package flash.display;

extern class AVM1Movie extends DisplayObject {
	function new() : Void;
	function addCallback(functionName : String, closure : flash.utils.Function) : Void;
	function call(functionName : String, restArgs : haxe.extern.Rest<Dynamic>) : Dynamic;
}
