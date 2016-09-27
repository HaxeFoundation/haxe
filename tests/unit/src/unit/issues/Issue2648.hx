package unit.issues;
import unit.Test;

#if java
private class TestParam<T> {
	@:overload static public function forName(s:Int) { }
	@:overload static public function forName(s:String) { }
}
#end

class Issue2648 extends Test {
	#if java
	function test() {
		TestParam.forName(1);
		TestParam.forName("s");
	}
	#end
}
