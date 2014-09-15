package unit.issues;

private abstract A(Int) {}
private abstract B(Int) from Int {}
private abstract C(Int) to Int {}
private abstract D(Int) from C to B {}

private abstract Arr<T>(Dynamic) from Array<T> to Array<T> {}
private abstract Brr<T>(Dynamic) from Array<Int> to Array<Int> {}

class Issue2063 extends Test {
	function test() {
		var x:{a:Int} = {a:9};
		var y:{a:A} = {a:untyped 9};
		var z:{a:B} = {a:untyped 9};
		var w:{a:C} = {a:untyped 9};
		t(unit.TestType.typeError(x = y));
		t(unit.TestType.typeError(y = x));
		t(unit.TestType.typeError(x = z));
		z = x;
		x = w;
		t(unit.TestType.typeError(w = x));

		// should fail (transitive cast)
		t(unit.TestType.typeError(z = w));

		// should succeed
		var p = {xs:[0,1,2]};
		var q:{xs:Arr<Int>} = p;
		p = q;
		var p = {xs:[0.0,1.0,2.0]};
		var q:{xs:Arr<Float>} = p;
		p = q;

		// should fail (transitive cast)
		var p = {xs:[0,1,2]};
		t(unit.TestType.typeError(var q:{xs:Arr<Float>} = p));
		t(unit.TestType.typeError(p = q));

		// should succeed
		var p = {xs:[0,1,2]};
		var q:{xs:Brr<Int>} = p;
		p = q;

		// should fail (wrong param)
		var p = {xs:[0.0,1.0,2.0]};
		t(unit.TestType.typeError(var q:{xs:Brr<String>} = p));
		t(unit.TestType.typeError(p = q));
	}
}