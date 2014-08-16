package unit.issues;

class Issue2868 extends Test {
	function test() {
		@:import(haxe.Serializer) {
			eq("n", Serializer.run(null));
		}
		t(unit.TestType.typeError(Serializer.run(null)));

		@:import(haxe.Serializer.run) {
			eq("n", run(null));
		}
		t(unit.TestType.typeError(run(null)));

		@:import(haxe.Serializer.run in r) {
			eq("n", r(null));
		}
		t(unit.TestType.typeError(r(null)));

		@:using(StringTools) {
			eq("n", "x".replace("x", "n"));
		}
		t(unit.TestType.typeError("x".replace("x", "n")));

		@:using(StringTools.StringTools) {
			eq("n", "x".replace("x", "n"));
		}
		t(unit.TestType.typeError("x".replace("x", "n")));
	}
}