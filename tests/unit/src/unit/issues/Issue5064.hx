package unit.issues;

import haxe.ds.Option;

private enum GADT<A> {
	AnInt(p: Bool): GADT<Int>;
	AString(p: Bool): GADT<String>;
}

class Issue5064 extends Test {
	public static function broken<A>(gadt: GADT<A>): Option<A> {
		return switch gadt {
			case AnInt(p): if (p) Some(1) else None;
			case AString(p): if (p) Some("a") else None;
		};
	}

	function test() {
		t(Type.enumEq(Some(1), broken(AnInt(true))));
		t(None == broken(AnInt(false)));
		t(Type.enumEq(Some("a"), broken(AString(true))));
		t(None == broken(AString(false)));
	}
}