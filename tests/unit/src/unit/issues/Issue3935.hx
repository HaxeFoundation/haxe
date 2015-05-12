package unit.issues;

private class MyClass {
	public function new() { }
}

class Issue3935 extends Test {
	function test() {
		var c:haxe.ds.IntMap<Dynamic> = [1=>2, 3=>"4"];
		var c:haxe.ds.StringMap<Dynamic> = ["1"=>2, "3"=>"4"];
		var m = new MyClass();
		var m2 = new MyClass();
		var c:haxe.ds.ObjectMap<MyClass, Dynamic> = [m => 1, m2 => "1"];
		var c:haxe.ds.EnumValueMap<haxe.macro.Expr.ExprDef, Dynamic> = [EBreak => 1, EContinue => "2"];
	}

	function testMap() {
		var c:Map<Int, Dynamic> = [1=>2, 3=>"4"];
		var c:Map<String, Dynamic> = ["1"=>2, "3"=>"4"];
		var m = new MyClass();
		var m2 = new MyClass();
		var c:Map<MyClass, Dynamic> = [m => 1, m2 => "1"];
		var c:Map<haxe.macro.Expr.ExprDef, Dynamic> = [EBreak => 1, EContinue => "2"];
	}

	function testFail() {
		t(unit.TestType.typeError([1=>2, 3=>"4"]));
		t(unit.TestType.typeError(["1"=>2, "3"=>"4"]));
		var m = new MyClass();
		var m2 = new MyClass();
		t(unit.TestType.typeError([m => 1, m2 => "1"]));
		t(unit.TestType.typeError([EBreak => 1, EContinue => "2"]));
	}
}