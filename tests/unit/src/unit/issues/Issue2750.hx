package unit.issues;
import unit.Test;

private class Parent extends Test {
	function new() {
		super();
	}

	function foo() {
		return 1;
	}
	function bind() {
		return 1;
	}
	function match() {
		return 1;
	}
}

class Issue2750 extends Parent {
	function test() {
		new Issue2750();
	}

	public function new() {
		super();
		eq(3, foo());
		eq(3, bind());
		eq(3, match());
		call(this);
		t(unit.HelperMacros.typeError(var x = super));
		t(unit.HelperMacros.typeError(call(super)));
		t(unit.HelperMacros.typeError({ field: super }));
		t(unit.HelperMacros.typeError([super]));
	}

	override function foo() {
		return 2 + super.foo();
	}

	override function bind() {
		return 2 + super.bind();
	}

	override function match() {
		return 2 + super.match();
	}

	function call(c:Parent) { }
}