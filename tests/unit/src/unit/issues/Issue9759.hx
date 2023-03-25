package unit.issues;

class Issue9759 extends unit.Test {
	var e = RootEnum;
	function test() {
		eq('RootEnum', Type.getEnumName(e));
	}
}