package flash.sampler;

@:final extern class StackFrame {
	var file(default,never) : String;
	var line(default,never) : UInt;
	var name(default,never) : String;
	@:require(flash10_1) var scriptID(default,never) : Float;
	function toString() : String;
}
