package unit.issues;

@:build(unit.issues.misc.Issue6873Macro.build())
private class C { }


class Issue6873 extends unit.Test {
	function test() {
		var expected = "TType(Map,[TInst(String,[]),TAbstract(Int,[])])TInst(haxe.ds.StringMap,[TAbstract(Int,[])])";
		eq(expected, C.result);
	}
}