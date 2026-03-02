package cases.display.issues;

class Issue11422 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var string = "";
				foo(0.0, s{-1-});
			}

			static function foo(a:Int, name:String):Void {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> item.kind == Local && item.args.name == "string");
	}

	/**
		class Main {
			static function main() {
				var string = "";
				foo(0.0, s{-1-});
			}

			overload static function foo(a:Int, name:String):Void {}
			overload static function foo(a:Bool, name:String):Void {}
		}
	**/
	function testOverload(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> item.kind == Local && item.args.name == "string");
	}
}
