package unit.issues;

class Issue4481 extends unit.Test {
	static var s:String = null;
#if !jvm
	function test() {
		switch s {
			case 'a': assert();
			case _: eq(null, s);
		}
	}
#end
}