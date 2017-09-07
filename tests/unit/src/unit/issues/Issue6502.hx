package unit.issues;

class Issue6502 extends unit.Test {
	function test() {
		var obj:IChild = new Dummy();
		eq(1, obj.field);
	}
}

private class Dummy implements IChild {
    public var field(get,never):Int;

    function get_field() return 1; //this getter is not generated

    public function new() {}
}

private interface IParent1 {
    var field(get,never):Int;
}

private interface IParent2 {}

private interface IChild extends IParent1 extends IParent2 {}