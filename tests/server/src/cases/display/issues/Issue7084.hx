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
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.equals(0, result.items.length);
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
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case EnumAbstractField: item.args.field.name == "Value";
			case _: false;
		});
	}
}
