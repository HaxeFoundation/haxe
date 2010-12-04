package flash.sampler;

@:final extern class StackFrame {
	var file : String;
	var line : UInt;
	var name : String;
	function toString() : String;
}
