package cases.display.issues;

class Issue8078 extends DisplayTestCase {
	/**
		extern class StringBuilder {
			@:overload(function new(s:String) {})
			public function new() { }

			public function append():Void { }
		}
		class Main {
			static function main() {
				var b = new StringBuilder("hi");
				b.{-1-}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "append";
			case _: false;
		});
	}
}
