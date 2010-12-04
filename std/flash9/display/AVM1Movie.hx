package flash.display;

extern class AVM1Movie extends DisplayObject {
	function new() : Void;
	function addCallback(functionName : String, closure : Dynamic) : Void;
	function call(functionName : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Dynamic;
}
