package unit.issues;

private typedef Struct = {
	var a: Int;
}

private typedef OtherStruct = {
	var b: Int;
}

private abstract Abstract(Struct) from Struct to Struct {

	public function new(t:Struct) this = t;

	public function getA() {
		return this.a;
	}

	@:to public function toOtherStruct():OtherStruct return { b: this.a };

	@:from public static function fromOtherStruct(f:OtherStruct)
		return new Abstract({ a: f.b });
}


class Issue3437 extends Test {
	function test() {
		var x:Abstract = { b: 5 };
		eq(5, x.getA());
	}
}