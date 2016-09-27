package unit.issues;

private class Object extends Node<Object,Ar<Object>> { }

@:generic
private class Node<T,Array_t:Ar<T>> {
	public var ar:Array_t;
}

@:generic
private class Ar<T> {
	public var nongen:Array<T>;
	public function length():Void {
		nongen.length;
	}
}

class Issue2085 extends Test {
	function test() {

	}
}