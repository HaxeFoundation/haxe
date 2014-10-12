package unit.issues;

import unit.issues.misc.Issue3089Macro;

@:genericBuild(unit.issues.misc.Issue3089Macro.build())
private class GenericBuild<Rest> { }

class Issue3089 extends Test {
	function test() {
		unit.TestType.typedAs((null:GenericBuild), (null:GenericBuildResult<"">));
		unit.TestType.typedAs((null:GenericBuild<String>), (null:GenericBuildResult<"String">));
		unit.TestType.typedAs((null:GenericBuild<String, Int, Float>), (null:GenericBuildResult<"String_Int_Float">));
	}
}