package unit.issues;

using unit.issues.Issue9737.Extension;

class Extension {
	extern overload static public inline function extend(i:Int) {
		return 'Int: $i';
	}

	extern overload static public inline function extend(s:String) {
		return 'String: $s';
	}
}

class Issue9737 extends unit.Test {
	function test() {
		eq("Int: 1", 1.extend());
		eq("String: foo", "foo".extend());
	}
}
