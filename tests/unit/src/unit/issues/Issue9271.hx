package unit.issues;

import unit.HelperMacros.typeString;

class Issue9271 extends unit.Test {
	function test() {
		var a:A = null;
		var b:B = null;
		var c:C = null;
		eq("String", typeString(a));
		eq("String", typeString(b));
		eq("Array<Int>", typeString(c));
	}
}

@:genericBuild(unit.issues.misc.Issue9271Macro.build())
private class A {}

@:genericBuild(unit.issues.misc.Issue9271Macro.Issue9271Macro.build())
private class B {}

@:genericBuild(unit.issues.misc.Issue9271Macro.Issue9271MacroSub.build())
private class C {}
