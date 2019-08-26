package flash.sampler;

extern final class StackFrame {
	final file : String;
	final line : UInt;
	final name : String;
	@:require(flash10_1) final scriptID : Float;
	function toString() : String;
}
