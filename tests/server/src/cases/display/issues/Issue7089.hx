package cases.display.issues;

class Issue7089 extends DisplayTestCase {
	/**
		enum abstract Foo(Int) {
			var Value = 1;
		}

		class Main {
			static function main() {
				Fo{-1-}o;
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.isTrue(result.result.item.type.kind == (cast "TAbstract" : Dynamic));
		Assert.equals("Foo", result.result.item.type.args.path.typeName);
	}
}
