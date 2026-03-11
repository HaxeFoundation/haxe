package cases.display.issues;

class Issue7248 extends DisplayTestCase {
	/**
		abstract Foo(Int) {
			static public function AStatic(){}
			public function NonStatic(){}
		}
		class Main {
			static function main() {
				Foo.{-1-}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "AStatic";
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "NonStatic";
			case _: false;
		});
	}
}
