package cases.display.issues;

class Issue9266 extends DisplayTestCase {
	/**
		class Main {
			static public function main() {
				te{-1-}st;
			}

			static public inline function test() {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover().result;
		Assert.equals('MethInline', result.item.args.field.kind.args);
	}
}
