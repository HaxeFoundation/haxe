package unit.issues;

class Issue8050 extends unit.Test {
	var field(get,never):Null<Int>;

	static var sideEffectCounter = 0;

	function get_field() {
		sideEffectCounter++;
		return 42;
	}

	function test() {
		var s = switch this {
			case {field: 42}: "yes";
			case _: "no";
		}
		eq("yes", s);
		eq(1, sideEffectCounter);
	}
}