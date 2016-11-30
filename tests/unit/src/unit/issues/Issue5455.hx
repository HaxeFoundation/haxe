package unit.issues;

class Issue5455 extends unit.Test {
	function test() {
		var s = protect('"foo"');
		var p = protect(0);
		var c = s.charAt(p++);
		var n = s.substring(p, s.indexOf(c, p));
		eq("foo", n);
	}

	static function protect<T>(v:T):T return v;
}