package cases.display.issues;

class Issue7055 extends DisplayTestCase {
	/**
		enum TestEnum {
			Some;
			Random;
			Enum;
			Constructors;
			For;
			Testing;
		}

		class Main {
			static function main() {
				switch ((null:TestEnum)) {
					case {-1-}
				}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		var items = result.result.items;
		var i = 0;
		function nextIs(name:String) {
			Assert.isTrue(i < items.length);
			if (i < items.length) {
				switch items[i].kind {
					case EnumField: Assert.equals(name, items[i].args.field.name);
					case _: Assert.fail('Expected EnumField, got ${items[i].kind}');
				}
				i++;
			}
		}
		nextIs("Some");
		nextIs("Random");
		nextIs("Enum");
		nextIs("Constructors");
		nextIs("For");
		nextIs("Testing");
	}
}
