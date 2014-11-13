package unit.issues;

private abstract BoxedInt({ val : Int }) from { val : Int } {
	public function new (v: { val :Int }) this = v;
	@:to public function toInt():Int return this.val;
}

class Issue2184 extends Test {
	function test() {
		var test = function () return new BoxedInt({ val : 5 });
		t(unit.TestType.typeError(var z:Void->Int = test));
	}
}