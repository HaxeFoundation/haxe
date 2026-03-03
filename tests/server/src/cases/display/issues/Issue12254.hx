package cases.display.issues;

class Issue12254 extends DisplayTestCase {
	/**
		using Main.Tools;

		class C {
			public function new() {}
			@:noCompletion public function f() {}
		}

		class Tools {
			static public function f(c:C, s:String) {}
		}

		function main() {
			var c = new C();
			c.{-1-}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.equals(0, result.result.items.length);
	}
}
