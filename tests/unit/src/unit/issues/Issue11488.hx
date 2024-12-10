package unit.issues;

private class Parent {
	public var that:Parent;
	public var visible(default, set) = true;

	public function new()
		that = this;

	public function set_visible(v:Bool) {
		throw "set_visible was called";
	}

	public function drawRec() {}
}

private class Child extends Parent {
	override function drawRec() {
		@:bypassAccessor that.visible = false;
	}
}

class Issue11488 extends Test {
	function test() {
		var child = new Child();
		child.drawRec();
		f(child.visible);
	}
}
