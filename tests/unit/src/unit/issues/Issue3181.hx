package unit.issues;

@:enum private abstract A(Int) { }

class Issue3181 extends Test {
	function test() {
		var a:Null<A> = cast 1;
		t(unit.TestType.typeError(
			switch(a) { }
		));
	}
}