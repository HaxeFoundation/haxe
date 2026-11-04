@:dce
class ConstructorException extends haxe.Exception {
	function new() {
		super("custom");
	}
}

function test() {
	return null is ConstructorException;
}
