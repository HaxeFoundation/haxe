package unit.issues;

class Issue3383 extends Test {
#if cs
	function test() {
		var i:Int = cast null,
				f:Float = cast null,
				s:Single = cast null,
				i64:haxe.Int64 = cast null,
				span:cs.system.TimeSpan = null,
				ui:UInt = cast null,
				n:cs.system.Nullable_1<Int> = null;

			eq(i, cast null);
			eq(f, cast null);
			eq(s, cast null);
			eq(i64, cast null);
			eq(span, null);
			eq(ui, cast null);
			eq(n, null);
	}
#end
}

