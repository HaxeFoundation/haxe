package unit.issues;

@:analyzer(no_check_has_effect)
class Issue3440 extends Test {
	function test() {
		var a = "foo";
		function sideEffect() {
			a = "bar";
			return 0;
		}
		{ a:1, b:sideEffect() };
		eq("bar", a);
	}
}