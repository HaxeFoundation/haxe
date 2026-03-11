package cases.display.issues;

class Issue6951 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				new Test<"{-1-}">().foo();
			}
		}

		@:generic class Test<@:const T> {
			public function new() {}
			public function foo() {
				trace(T);
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals("String", result.item.type.args.path.typeName);
	}
}
