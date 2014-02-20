package unit.issues;
import unit.Test;

#if java
private class Test<T> {
	@:overload static public function forName(s:Int) { }
	@:overload static public function forName(s:String) { }
}
#end

class Issue2648 extends Test {
	#if java
	function test() {
		Test.forName(1);
		Test.forName("s");
	}
	#end
}