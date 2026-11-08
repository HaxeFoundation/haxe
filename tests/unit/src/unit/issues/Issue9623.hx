package unit.issues;

class Issue9623 extends unit.Test {
	function test() {
		final s = new String("abc");

		eq("abc", s);
		t(s == "abc");

		t(switch s {
			case "abc": true;
			default: false;
		});

		#if !js
		t(switch s {
			case "abc": true;
			case "def": false;
			default: false;
		});
		#end
	}
}
