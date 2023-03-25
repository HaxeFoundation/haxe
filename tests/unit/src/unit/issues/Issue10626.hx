package unit.issues;

macro function foo() {
	var s = try {
		haxe.macro.Context.typeExpr(macro var x:Void);
		"no exception";
	} catch (e) {
		"an exception";
	}
	return macro $v{s};
}

class Issue10626 extends unit.Test {
	function test() {
		eq("an exception", foo());
	}
}
