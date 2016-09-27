package unit.issues;

@:generic
private class Node<T:Node<T>> extends Foo<T> {
	public var node:T;
	public function new(node:T) {
		super();
		this.node = node;
		s += node.func();
		s += node.super_func();
	}

	public function func() {
		return "Node_func";
	}
}

@:generic
private class Foo<T> {
	public var s:String = "";
	public function new() { }
	public function super_func() {
		return "Foo_super_func";
	}
}

private class StringNode extends Node<StringNode> {
	public function new(s:String) {
		super(this);
	}

	override function func() {
		return "StringNode_func" + super.func();
	}
}

class Issue2086 extends Test {
	function test() {
		var sNode = new StringNode("foo");
		eq("StringNode_funcNode_funcFoo_super_func", sNode.s);
	}
}