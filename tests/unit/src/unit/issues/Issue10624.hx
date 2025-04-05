package unit.issues;

class Issue10624 extends unit.Test {
	static final RANDOM:Int = 33;

	public function test() {
		var v = switch (33) {
			case 1:
				"no";
			case RANDOM:
				"yes";
			default:
				"noooo";
		}
		eq("yes", v);
	}
}
