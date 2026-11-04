package cases.display.issues;

class Issue5709 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				var foo:F{-1-}oo = new F{-2-}oo();
			}
		}

		/** Foo doc **\/
		class Foo {
			public function new() {}
		}

		/** IBaz doc **\/
		interface IBaz {}

		class Bar extends F{-3-}oo {}
		class Baz implements I{-5-}Baz {}

		typedef Foos = Array<F{-4-}oo>;
	**/
	function test(_) {
		for (i in 1...5) {
			var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(i)});
			Assert.equals("Foo doc", StringTools.trim(result.item.args.doc));
		}
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(5)});
		Assert.equals("IBaz doc", StringTools.trim(result.item.args.doc));
	}
}
