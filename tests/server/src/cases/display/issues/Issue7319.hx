package cases.display.issues;

class Issue7319 extends DisplayTestCase {
	/**
		import haxe.ds.Option;

		class Main {
			static function main() {
				var option:Option<Int>;
				switch option {
					case None:
					case So{-1-}
				}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasNoCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "So";
			case _: false;
		});
	}
}
