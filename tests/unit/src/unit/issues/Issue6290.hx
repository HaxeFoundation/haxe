package unit.issues;

private class Parent {
	public function new() {}
}

private class Child extends Parent {}

private class TakeParent {
	public function new(parent:Parent) {}
}

private class TakeChild extends TakeParent {
	public function new(child:Child) {
		super(child);
		if (!Std.isOfType(child, Child)) throw 'wtf?';
	}
}

class Issue6290 extends unit.Test {
	@:generic static function get<Arg, Cls:haxe.Constraints.Constructible<Arg->Void>>(a:Arg, c:Class<Cls>) {
		return new Cls(a);
	}

	function test() {
		var parent = new Parent(),
		    child = new Child();

		get(parent, TakeParent);
		get(child, TakeChild);
		t(unit.HelperMacros.typeError(get(parent, TakeChild)));
		get(child, TakeParent);
	}
}