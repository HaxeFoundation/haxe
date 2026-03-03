package cases.display.issues;

class Issue7068 extends DisplayTestCase {
	/**
		enum Foo { Bar; }

		class Main {
			static function main() {
				switch ((null:Foo)) {
					case {-1-} if (false):
				}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.isTrue(result.result.items.length > 0);
		switch result.result.items[0].kind {
			case EnumField: Assert.equals("Bar", result.result.items[0].args.field.name);
			case _: Assert.fail('Expected EnumField');
		}
	}

	/**
		enum Foo { Bar; }

		class Main {
			static function main() {
				switch ((null:Foo)) {
					case Bar | {-1-} if (false):
				}
			}
		}
	**/
	function test2(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.isTrue(result.result.items.length > 0);
		switch result.result.items[0].kind {
			case EnumField: Assert.equals("Bar", result.result.items[0].args.field.name);
			case _: Assert.fail('Expected EnumField');
		}
	}
}
