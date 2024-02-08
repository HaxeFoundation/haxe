package unit.issues;

private abstract A(String) from String {
	@:resolve function resolve(s:String) {
		return () -> s;
	}
}

class Issue9745 extends unit.Test {
	function test() {
		var a:A = "foo";
		eq("field", a.field());
	}
}