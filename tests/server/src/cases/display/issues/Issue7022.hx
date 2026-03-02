package cases.display.issues;

class Issue7022 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {}

			public function ne{-1-}w() {
				new Main();
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.isTrue(result.result.item.type.kind == (cast "TFun" : Dynamic));
	}
}
