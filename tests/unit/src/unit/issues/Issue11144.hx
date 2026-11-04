package unit.issues;

class Issue11144 extends Test {
	function test() {
		final vfalse:Null<Bool> = false;
		final vtrue:Null<Bool> = true;
		t(vfalse ?? vfalse || vtrue);
		t((vfalse ?? vfalse) || vtrue);
	}
}
