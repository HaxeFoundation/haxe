package cases.display.issues;

class Issue5729 extends DisplayTestCase {
	/**
		enum TestEnum {
			Constructor(i:Int);
		}
		class Main {
			public static function main() {
				var c = Constructor(1);
				switch (c) {
					case Constructor(int{-1-}eger): trace("test");
				}
			}
		}
	**/
	function testType1(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("Int", result.result.item.type.args.path.typeName);
	}
}
