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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.isTrue(result.item.type.kind == (cast "TFun" : Dynamic));

		result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(3)});
		Assert.isTrue(result.item.type.kind == (cast "TFun" : Dynamic));
	}
}
