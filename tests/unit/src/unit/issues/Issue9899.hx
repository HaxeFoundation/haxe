package unit.issues;

class Issue9899 extends unit.Test {
	function test() {
		t(switch (macro !a is T) {
			case {expr: EIs({expr: EUnop(_)}, _)}: true;
			case _: false;
		});
	}
}