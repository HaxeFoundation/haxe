package cases.display.issues;

class Issue7171 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				Std.string.bi{-1-}nd(_);
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.isTrue(result.result.item.type.kind == (cast "TFun" : Dynamic));
	}
}
