package unit.issues;

import haxe.Constraints.Constructible;
import unit.Test;

private class Base<T> {
	public var value:T;
}

@:generic
private class InnerGeneric<T:Constructible<Void->Void>> {
	public var innerValue:T;

	public function new() {
		this.innerValue = new T();
	}
}

@:generic
private class OuterGeneric<T:Constructible<Void->Void>> extends Base<InnerGeneric<T>> {
	public function new() {
		this.value = new InnerGeneric();
	}
}

private class Foo {
	public function new() {}

	public function getValue() {
		return "value";
	}
}

class Issue6761 extends Test {
	function test() {
		var a = new OuterGeneric<Foo>();
		var b:Base<InnerGeneric<Foo>> = null;
		b = a;
		if (a.value != null) {
			eq("value", a.value.innerValue.getValue());
		}
	}
}
