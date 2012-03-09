package flash.sampler;

@:final extern class StackFrame {
	var file : String;
	var line : UInt;
	var name : String;
	@:require(flash10_1) var scriptID : Float;
	function toString() : String;
}
