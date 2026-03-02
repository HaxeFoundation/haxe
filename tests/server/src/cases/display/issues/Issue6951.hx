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
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("String", result.result.item.type.args.path.typeName);
	}
}
