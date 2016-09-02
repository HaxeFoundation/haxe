package unit.issues;

private extern class C {
	public inline function inlineMe():String {
		return "I'm inlined!";
	}
}

private typedef T = {
	function inlineMe():String;
}

@:multiType
private abstract A(T) {

	public function new();

	@:to @:extern inline function toC():C {
		return null;
	}

	public inline function get(s:String) {
		return this.inlineMe();
	}
}

class Issue5544 extends unit.Test {
	function test() {
		eq("I'm inlined!", new A().get("foo"));
	}
}