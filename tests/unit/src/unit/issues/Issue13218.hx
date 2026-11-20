package unit.issues;

private class StringBox {
	public static function make<T:String>(v:T):{v:T}
		return {v: v};
}

private class AnyBox {
	public static function make<T>(v:T):{v:T}
		return {v: v};
}

class Issue13218 extends Test {
	function test() {
		eq("abc", StringBox.make("abc").v);
		eq(12, AnyBox.make(12).v);
	}
}
