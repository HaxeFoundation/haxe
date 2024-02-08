package unit.issues;

private enum E {
	CInt(i:Int);
	CString(s:String);
}

class Issue11157 extends Test {
	function process(e:E) {
		return switch e {
			case CInt(_ > 0 && _ < 12):
				"in range";
			case CString(_.toLowerCase() == "foo"):
				return "foo";
			case _:
				return "something else";
		}
	}

	function test() {
		eq("in range", (process(CInt(11))));
		eq("something else", (process(CInt(12))));
		eq("foo", (process(CString("FOO"))));
	}
}
