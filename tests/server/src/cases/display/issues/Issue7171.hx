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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.isTrue(result.item.type.kind == (cast "TFun" : Dynamic));
	}
}
