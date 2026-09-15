package unit.issues;

private abstract MyStringA(Null<String>) from Null<String> {
	function toString() {
		if (this == null)
			return "EMPTY";
		return this;
	}
}

private typedef NullableString = Null<String>;

private abstract MyStringB(NullableString) from NullableString {
	function toString() {
		if (this == null)
			return "EMPTY";
		return this;
	}
}

class Issue12019 extends unit.Test {
	final a:MyStringA = null;
	final b:MyStringB = null;

	var oldTrace:(Dynamic, ?Null<haxe.PosInfos>) -> Void;

	function setup() {
		oldTrace = haxe.Log.trace;
	}

	function teardown() {
		haxe.Log.trace = oldTrace;
	}

	function testTrace() {
		haxe.Log.trace = function(v, ?infos) {
			eq("EMPTY", v);
		};

		trace(a);
		trace(b);
	}

	function testConcatenate() {
		eq("Concatenated: EMPTY", "Concatenated: " + a);
		eq("Concatenated: EMPTY", "Concatenated: " + b);
	}
}
