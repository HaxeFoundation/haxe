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

private abstract MyStringAWrapper(MyStringA) from MyStringA {
	function toString() {
		return "MyStringAWrapper";
	}
}

private abstract MyAbstract<T>(T) from T {
	function toString() {
		return "MyAbstract";
	}
}

class Issue12019 extends unit.Test {
	final a:MyStringA = null;
	final b:MyStringB = null;
	final d:MyStringAWrapper = null;
	final c:MyAbstract<Null<String>> = null;

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

		haxe.Log.trace = function(v, ?infos) {
			eq("MyStringAWrapper", v);
		};

		trace(d);

		haxe.Log.trace = function(v, ?infos) {
			eq("MyAbstract", v);
		};

		trace(c);
	}

	function testConcatenate() {
		eq("Concatenated: EMPTY", "Concatenated: " + a);
		eq("Concatenated: EMPTY", "Concatenated: " + b);
		eq("Concatenated: MyStringAWrapper", "Concatenated: " + d);
		eq("Concatenated: MyAbstract", "Concatenated: " + c);
	}

	function testStdString() {
		eq("EMPTY", Std.string(a));
		eq("EMPTY", Std.string(b));
		eq("MyStringAWrapper", Std.string(d));
		eq("MyAbstract", Std.string(c));
	}
}
