package cases;

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
	function testStatic() {
		eq("(i : Int) -> Void", type(pos(1)));
		eq("(s : String) -> Void", type(pos(2)));
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
	function testInstance() {
		eq("(i : Int) -> Void", type(pos(1)));
		eq("(s : String) -> Void", type(pos(2)));
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
	function testConstructor() {
		eq("(i : Int) -> cases.Foo", type(pos(1)));
		eq("(s : String) -> cases.Foo", type(pos(2)));
	}
}
