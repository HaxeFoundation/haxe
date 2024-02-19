package unit.issues;

class Issue10106_CExtension {
	public static function fromS(cls:Class<Issue10106_C>, s:String) {
		return new Issue10106_C(s);
	}
}

@:using(unit.issues.Issue10106.Issue10106_CExtension)
class Issue10106_C {
	public final s:String;

	public function new(s:String) {
		this.s = s;
	}
}

class Issue10106_EnExtension {
	public static function fromS(en:Enum<Issue10106_En>, st:String):Issue10106_En {
		return A;
	}
}

@:using(unit.issues.Issue10106.Issue10106_EnExtension)
enum Issue10106_En {
	A;
	B;
}

class Issue10106 extends Test {
	function test() {
		eq(A, Issue10106_En.fromS("A"));
		eq("foo", Issue10106_C.fromS("foo").s);
	}
}
