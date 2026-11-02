package cases.display.issues;

class Issue7288 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				var f:Foo = 0;
				f.f{-1-}oo;
			}
		}

		abstract Foo(Int) from Int {
			public var foo(get, never):Int;
			function get_foo() return 0;
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.isTrue(switch result.item.kind {
			case ClassField: result.item.args.field.name == "foo";
			case _: false;
		});
	}
}
