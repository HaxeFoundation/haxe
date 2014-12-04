package unit.issues;

private typedef Option = {
	?foo: Int,
	?bar: Int,
}

class Issue3547 extends Test {
	function test() {
		var o = {foo:12, "x-bar":13};
		assign(o);
		assign({foo:12, "x-bar":13});
	}

	static function assign(o:Option) { }
}