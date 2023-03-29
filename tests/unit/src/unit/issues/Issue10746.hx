package unit.issues;

class Issue10746 extends Test {
#if js
	@:keep var _ = @:privateAccess js.Boot.getClass;

	function test() {
		js.Syntax.code("String.prototype.__class__ = String");
		t(true);
	}
#end
}

