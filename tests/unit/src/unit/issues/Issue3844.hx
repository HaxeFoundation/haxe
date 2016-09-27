package unit.issues;

private typedef A = {f: A}

class Issue3844 extends Test {
	function test() {
		var n:A = {f: null};
		do n = n.f while (n!=null && (n!=null || n!=null));
	}
}