package cases.display.issues;

class Issue10106 extends DisplayTestCase {
	/**
		class CExtension {
			public static function toS(c: C): String {
				return 'c';
			}
			public static function fromS(cls: Class<C>, s: String):C {
				return new C();
			}
		}

		@:using(Main.CExtension)
		class C {
			public function new(){}
		}


		class Main {
			static public function main() {
				C.{-1-}
			}
		}
	**/
	function testClass(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "fromS";
			case _: false;
		});
	}

	/**
		class EnExtension {
			public static function toS(e:En):String {
				return '${e}';
			}

			public static function fromS(en:Enum<En>, s:String):En {
				return A;
			}
		}

		@:using(Main.EnExtension)
		enum En {
			A;
			B;
		}


		class Main {
			static public function main() {
				En.{-1-}
			}
		}
	**/
	function testEnum(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "fromS";
			case _: false;
		});
	}
}
