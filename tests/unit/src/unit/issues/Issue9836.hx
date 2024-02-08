package unit.issues;

private class Parent {
	public function new() {}

	public function contravariant(i:Int):Float {
		return 0.0;
	}

	public function covariant():Float {
		return 0.0;
	}

	public function variant(i:Int):Float {
		return 0.0;
	}
}

private class Child extends Parent {
	override function contravariant(i:Float):Float {
		return 1.0;
	}

	override function covariant():Float {
		return 1.0;
	}

	override function variant(f:Float):Int {
		return 1;
	}
}

class Issue9836 extends unit.Test {
	function test() {
		var child = new Child();
		var parent:Parent = child;
		feq(1.0, parent.contravariant(0));
		feq(1.0, child.contravariant(0));
		feq(1.0, parent.covariant());
		feq(1.0, child.covariant());
		feq(1.0, parent.variant(0));
		feq(1.0, child.variant(0));
	}
}
