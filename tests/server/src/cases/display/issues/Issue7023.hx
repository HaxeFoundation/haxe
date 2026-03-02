package cases.display.issues;

class Issue7023 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				inl{-1-}ine funct{-2-}ion fo{-3-}o() return "foo";
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		var result = parseHover();
		Assert.equals("TFun", result.result.item.type.kind);

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)});
		result = parseHover();
		Assert.equals("TFun", result.result.item.type.kind);
	}
}
