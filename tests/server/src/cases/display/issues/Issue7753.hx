package cases.display.issues;

class Issue7753 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				Foo.f{-1-}oo(0);
				Foo.f{-2-}oo("");
			}
		}

		extern class Foo {
			@:overload(function(s:String):Void {})
			static function foo(i:Int):Void;
		}
	**/
	function testStatic(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.isTrue(parseHover().result.item.type.kind == (cast "TFun" : Dynamic));

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.isTrue(parseHover().result.item.type.kind == (cast "TFun" : Dynamic));
	}

	/**
		class Main {
			static function main() {
				var foo = new Foo();
				foo.f{-1-}oo(0);
				foo.f{-2-}oo("");
			}
		}

		extern class Foo {
			function new():Void;
			@:overload(function(s:String):Void {})
			function foo(i:Int):Void;
		}
	**/
	function testInstance(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.isTrue(parseHover().result.item.type.kind == (cast "TFun" : Dynamic));

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.isTrue(parseHover().result.item.type.kind == (cast "TFun" : Dynamic));
	}

	/**
		class Main {
			static function main() {
				var foo = n{-1-}ew Foo(0);
				var foo = n{-2-}ew Foo("");
			}
		}

		extern class Foo {
			@:overload(function(s:String):Void {})
			function new(i:Int):Void;
		}
	**/
	function testConstructor(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.isTrue(parseHover().result.item.type.kind == (cast "TFun" : Dynamic));

		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(2)});
		Assert.isTrue(parseHover().result.item.type.kind == (cast "TFun" : Dynamic));
	}
}
