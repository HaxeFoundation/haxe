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
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.isTrue(result.items.length > 0);
		switch result.items[0].kind {
			case EnumField: Assert.equals("Bar", result.items[0].args.field.name);
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
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.isTrue(result.items.length > 0);
		switch result.items[0].kind {
			case EnumField: Assert.equals("Bar", result.items[0].args.field.name);
			case _: Assert.fail('Expected EnumField');
		}
	}
}
