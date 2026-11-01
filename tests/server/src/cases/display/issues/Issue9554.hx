package cases.display.issues;

class Issue9554 extends DisplayTestCase {
	/**
		using Main.Main;

		class Main {
			static public function main() {
				infer({foo: 12});
			}

			static function infer(a) {
				a.foo = 12;
				a.{-1-}
			}

			static function staticExtension(a:{foo:Int}) {}
		}

	**/
	function testCatch_noTypeHint(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.equals(1, result.items.length);
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "foo" && item.args.field.kind.kind == FVar;
			case _: false;
		});
	}

	/**
		using Main.Main;

		class Main {
			static public function main() {
				var a:{foo:Int} = {foo: 12};
				a.{-1-}
			}

			static function staticExtension(a:{foo:Int}) {}
		}

	**/
	function testStaticExtension(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "foo" && item.args.field.kind.kind == FVar;
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "staticExtension";
			case _: false;
		});
	}
}
