package unit.issues;

class Issue12205 extends unit.Test {
	function test() {
		eq("no rest foo", over("foo"));
		eq("rest foo bar", over("foo", "bar"));
	}

	overload extern static inline function over(s:String) {
		return "no rest " + s;
	}

	overload extern static inline function over(s:String, ...r:String) {
		return "rest " + s + " " + r.toArray().join(" ");
	}
}
