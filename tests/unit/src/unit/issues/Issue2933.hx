package unit.issues;

private abstract A(Int) from Int to Int {}

private abstract B({a:A}) from {a:A} {
	public function getA() {
		return this.a;
	}
}

private typedef T = {a:A};

private abstract C(T) from T {
	public function getA() {
		return this.a;
	}
}

class Issue2933 extends Test {
	function test() {
		var a:A = 1;
		var b:B = {a: 1};
		eq(1, b.getA());

		var c:C = {a: 1};
		eq(1, c.getA());
	}
}