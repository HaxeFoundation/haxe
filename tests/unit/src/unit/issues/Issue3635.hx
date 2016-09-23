package unit.issues;

private typedef A = { var x:Int; };
private typedef B = { var x(default, never):Int; };

class Issue3635 extends Test {
	function test() {
		var a:A, b:B;
		var dA:Dynamic<A>, dB:Dynamic<B>;

		a = { x: 10 };
		b = { x: 10 };

		dA = { key: { x: 10 }};
		dB = { key: { x: 10 }};
		dB = {};
		dB.key = { x: 10 };
		dB = { key: b };

		t(unit.HelperMacros.typeError(dA = { key: { y: 120 } }));
		t(unit.HelperMacros.typeError(dB = { key: { y: 120 } }));
	}
}