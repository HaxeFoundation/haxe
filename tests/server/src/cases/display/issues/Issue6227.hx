package cases.display.issues;

class Issue6227 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
			new Foo(4) {-1-}> new Foo(3);
			}
		}

		abstract Foo({i:Int}) {
			public function new(i) this = {i: i};
			@:op(a > b) function f(other:Foo):String return "true";
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.equals("String", result.result.item.type.args.path.typeName);
	}
}
