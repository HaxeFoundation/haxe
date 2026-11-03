package cases.display.issues;

class Issue7224 extends DisplayTestCase {
	/**
		typedef Nope = Int;

		class Bar {
			public function new() {}
		}
		typedef Foo = Bar;

		class Main {
			static function main() {
				new {-1-}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Nope";
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Bar";
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Foo";
			case _: false;
		});
	}
}
