package unit.issues;

private abstract A(Array<Float>) to Array<Float> {}
private abstract B(Array<A>) to Array<Array<Float>> {}
private abstract C(Array<B>) to Array<Array<Array<Float>>> {}

class Issue5342 extends unit.Test {
	function test() {
		var c:C = cast [[[1.5]]];
		var x:Array<Array<Array<Float>>> = c;
		eq(1.5, x[0][0][0]);
	}
}
