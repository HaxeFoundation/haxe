package cases.display.issues;

class Issue7136 extends DisplayTestCase {
	/**
		@:structInit class Point<T> {
			var x:Int;
			var y:T;
		}

		class Main {
			static function main() {
				var p:Point<String> = {
					{-1-}
				}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.equals(2, result.items.length);
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "x";
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "y";
			case _: false;
		});
	}
}
