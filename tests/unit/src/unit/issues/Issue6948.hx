package unit.issues;

class Issue6948 extends Test {
	function test() {
		var gen:Gen<true> = null;
	}
}

@:generic
class Gen<@:const T>{}
