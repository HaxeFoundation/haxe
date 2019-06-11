package unit.issues;

class Issue7546 extends unit.Test {
	function test() {
		var instance:Dynamic = {};
		eq(null, instance.field);

		eq(null, ({}:Dynamic).timeout);

		var instance = ({}:Dynamic);
		eq(null, instance.field);
	}
}