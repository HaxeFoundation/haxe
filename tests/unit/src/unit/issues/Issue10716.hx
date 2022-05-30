package unit.issues;

using unit.issues.Issue10716.Extension;

private class Extension {
	macro public static function test(x:ExprOf<Int>)
		return macro $x + $v{1};
}

class Issue10716 extends unit.Test {
	function test() {
		#if !macro
		eq(11, 10.test());
		#end

		utest.Assert.pass();
	}
}

function test()
	return 1;
