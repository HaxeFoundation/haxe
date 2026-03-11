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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.equals('MethInline', result.item.args.field.kind.args);
	}
}
