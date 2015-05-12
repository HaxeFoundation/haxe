package unit.issues;

private enum ValueEnum {
	VString(v : String);
	VInt(v : Int);
}

private abstract ValueAbs(ValueEnum) {
	public inline function new(v : ValueEnum) {
		this = v;
	}

	@:from
	public static function fromString(v : String) : ValueAbs {
		return new ValueAbs(VString(v));
	}

	@:from
	public static function fromInt(v : Int) {
		return new ValueAbs(VInt(v));
	}

	public function getString() {
		return switch (this) {
			case VString(s): s;
			case VInt(i): "" + i;
		}
	}
}

class Issue4100 extends Test {
	function test() {
		var myMap2 : Map<String, ValueAbs> = [
		  "one" => "some other string",
		  "two" => 12
		];
		eq("some other string", myMap2["one"].getString());
		eq("12", myMap2["two"].getString());
	}
}