package flash.display;

extern class AVM1Movie extends flash.display.DisplayObject {
	function new() : Void;
	function addCallback(functionName : String, closure : Function) : Void;
	function call(functionName : String /* ...arguments */) : Void;
	private function _callAS2(functionName : String, arguments : flash.utils.ByteArray) : flash.utils.ByteArray;
	private function _callAS3(functionName : String, data : flash.utils.ByteArray) : Void;
	private var _interopAvailable(default,null) : Bool;
	private function _setCallAS3(closure : Function) : Void;
	private var callbackTable : Dynamic;
}
