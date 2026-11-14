package unit.issues;

class Issue9623 extends unit.Test {
	function checkEquality(s) {
		eq("abc", s);
		t(s == "abc");

		t(switch s {
			case "abc": true;
			default: false;
		});

		t(switch s {
			case "abc": true;
			case "def": false;
			default: false;
		});
	}

	function testDirectCall() {
		checkEquality(new String("abc"));
	}

	function testReflectiveCall() {
		checkEquality(Type.createInstance(String, ["abc"]));

		final empty = Type.createEmptyInstance(String);

		#if (!js && !neko && !lua) // #13164
		eq("", empty);
		t(empty == "");

		t(switch empty {
			case "": true;
			default: false;
		});

		t(switch empty {
			case "": true;
			case "def": false;
			default: false;
		});
		#end
	}
}
