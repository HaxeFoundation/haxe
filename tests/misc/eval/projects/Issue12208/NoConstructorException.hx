@:dce
class NoConstructorException extends haxe.Exception {}

function test() {
	return null is NoConstructorException;
}
