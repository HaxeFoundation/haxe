package unit.issues;

private class MyClass {
	@:overload(function (x:Int):String {})
	@:overload(function (x:String, y:Bool):Int {})
	function test() { }
}

class Issue3460 extends Test {
	function test() {
		eq("x : Int -> String, x : String -> y : Bool -> Int", unit.issues.misc.Issue3460Macro.getOverloadString((null : MyClass)));
	}
}