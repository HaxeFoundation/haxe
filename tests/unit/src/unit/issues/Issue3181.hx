package unit.issues;

private enum abstract A(Int) { }

class Issue3181 extends Test {
	function test() {
		var a:Null<A> = cast 1;
		t(unit.HelperMacros.typeError(
			switch(a) { }
		));
	}
}