package unit.issues;

@:native("some")
private class C {
	public var v = true;
	public function new() {}
}

class Issue6448 extends unit.Test {
	#if js
	@:analyzer(no_local_dce)
	function test() {
		var some = null;
		t(new C().v);
	}
	#end
}
