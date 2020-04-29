package cases.display.issues;

class Issue8381 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var f:Foo;
				f.f{-1-}oo();
				f.bar;
			}
		}

		typedef Foo = {
			function foo():Void;

			var bar:Int;
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(1)
		});
		var result = parseHover();
		Assert.equals(DisplayItemKind.ClassField, result.result.item.kind);
	}
}