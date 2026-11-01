package cases.display.issues;

class Issue6004 extends DisplayTestCase {
	/**
		class Main {
			static function f(a:Int) return a;

			static function main() {
				f.{-1-}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "bind";
			case _: false;
		});
	}
}
