package unit.issues;

@:nativeGen
private class Some {
	public var s:String;
	public function new() s = "hello";
}

class Issue5481 extends Test {
	#if !cpp
	function test() {
		var o = new Some();
		Reflect.setField(o, "s", null);
		eq(o.s, null);
	}
	#end
}
