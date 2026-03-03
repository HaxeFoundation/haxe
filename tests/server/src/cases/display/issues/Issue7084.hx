package cases.display.issues;

class Issue7084 extends DisplayTestCase {
	/**
		enum abstract Foo(Int) {
			var Value = 0;
		}

		class Main {
			public static function main() {
				var f:Foo;
				f.{-1-}
			}
		}
	**/
	function test1(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.equals(0, result.result.items.length);
	}

	/**
		enum abstract Foo(Int) {
			var Value = 0;
		}

		class Main {
			public static function main() {
				Foo.{-1-};
			}
		}
	**/
	function test2(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case EnumAbstractField: item.args.field.name == "Value";
			case _: false;
		});
	}
}
