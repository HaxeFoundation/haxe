package unit.issues;

private enum abstract A(Int) { }

class Issue3181 extends Test {

	/* Note: This no longer fails at block-level because it shouldn't. It does fail at
	   in value places because we type the switch as Void in that case. However, we cannot
	   test that here because this check is made at a later stage. */

	// function test() {
	// 	var a:Null<A> = cast 1;
	// 	t(unit.HelperMacros.typeError(
	// 		var b = switch(a) { }
	// 	));
	// }
}