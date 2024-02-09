package cases.display.issues;

class Issue9318 extends DisplayTestCase {
	/**
		typedef T = {
			var {-1-}field{-2-}:Int;
		}

		@:structInit
		class C {
			public function new({-3-}field{-4-}:Int) {}
		}

		@:structInit
		class D {
			var {-5-}field{-6-}:Int;
		}

		class Main {
			static function main() {
				var t:T = {fie{-7-}ld: 1}; // clicking on `field` takes to the field position: ideal
				var c:C = {fie{-8-}ld: 1}; // clicking on `field` takes to the contstructor position: not bad, but ideally should take to the argument position (not sure if possible)
				var d:D = {fiel{-9-}d: 1}; // clicking on `field` doesn't work :()
			}
		}
	**/
	function test(_) {
		var args = ["Main", "-js", "main.js"];
		function parseGotoDefintion():GotoDefinitionResult {
			return haxe.Json.parse(lastResult.stderr).result.result.map(result -> result.range);
		}
		runHaxeJson(args, DisplayMethods.GotoDefinition, {file: file, offset: offset(7), contents: source});
		Assert.same([range(1, 2)], parseGotoDefintion());
		// runHaxeJson(args, DisplayMethods.GotoDefinition, {file: file, offset: offset(8), contents: source});
		// Assert.same([range(3, 4)], parseGotoDefintion());
		runHaxeJson(args, DisplayMethods.GotoDefinition, {file: file, offset: offset(9), contents: source});
		Assert.same([range(5, 6)], parseGotoDefintion());
	}
}
