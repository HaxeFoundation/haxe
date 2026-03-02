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
		// Hovering over an enum abstract gives the meta-type "Abstract<Foo>"
		Assert.equals("Abstract<Foo>", result.result.item.type.args.path.typeName);
	}
}
