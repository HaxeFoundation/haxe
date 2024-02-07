package unit.issues;

private class CExtension {
	public static function fromS(cls:Class<C>, s:String) {
		return new C(s);
	}
}

@:using(unit.issues.Issue10106.CExtension)
private class C {
	public final s:String;

	public function new(s:String) {
		this.s = s;
	}
}

private class EnExtension {
	public static function fromS(en:Enum<En>, st:String):En {
		return A;
	}
}

@:using(unit.issues.Issue10106.EnExtension)
private enum En {
	A;
	B;
}

class Issue10106 extends Test {
	function test() {
		eq(A, En.fromS("A"));
		eq("foo", C.fromS("foo").s);
	}
}
