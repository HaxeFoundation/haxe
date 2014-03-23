package unit.issues;
import unit.Test;

private class Parent extends Test {
    function new() {
		super();
	}
	
	function foo() {
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
		call(this);
		t(unit.TestType.typeError(var x = super));
		t(unit.TestType.typeError(call(super)));
		t(unit.TestType.typeError({ field: super }));
		t(unit.TestType.typeError([super]));
    }
	
	override function foo() {
		return 2 + super.foo();
	}
	
	function call(c:Parent) { }
}