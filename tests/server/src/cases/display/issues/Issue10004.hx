package cases.display.issues;

class Issue10004 extends DisplayTestCase {
	/**
		using Main.Foo;

		class Main {
			static function main() {
				0.{-1-}
			}
		}

		class Foo {
			public static overload extern inline function foo():Void {}

			public static overload extern inline function foo(i:Int):Void {}
		}
	**/
	function testGama(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "foo";
			case _: false;
		});
	}

	/**
		using Main.Tools;

		class Tools {
			public static extern inline overload function getOrZero<K>(map:Map<K,Int>, key:K):Int {
				var value = map.get(key);
				return if (value != null) value else 0;
			}

			public static extern inline overload function getOrZero<K>(map:Map<K,Float>, key:K):Float {
				var value = map.get(key);
				return if (value != null) value else 0.0;
			}

		}

		function main() {
			var m = ["a" => 1.1, "b" => 2.3];
			m.{-1-}
		}
	**/
	function testNotGama(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "getOrZero";
			case _: false;
		});
	}
}
