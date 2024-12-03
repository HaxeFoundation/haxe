package unit.issues;
import unit.Test;

#if jvm
private class TestParam<T> {
	@:overload static public function forName(s:Int) { }
	@:overload static public function forName(s:String) { }
}
#end

class Issue2648 extends Test {
	#if jvm
	function test() {
		TestParam.forName(1);
		TestParam.forName("s");
		noAssert();
	}
	#end
}
