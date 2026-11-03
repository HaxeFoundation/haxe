package unit.teststd.haxe.macro;

class TestComplexTypeTools extends unit.Test {
	public function test() {
		#if macro
		var tt = function(c) return Std.string(haxe.macro.ComplexTypeTools.toType(c));
		eq(tt(macro : String), "String");
		eq(tt(macro : Int), "String");
		#else
		eq(1, 1);
		#end
	}
}
