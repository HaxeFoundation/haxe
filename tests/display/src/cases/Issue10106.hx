package cases;

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

		@:using(cases.Issue10106.CExtension)
		class C {
			public function new(){}
		}


		class Main {
			static public function main() {
				C.{-1-}
			}
		}
	**/
	function testClass() {
		eq(true, hasField(fields(pos(1)), "fromS", "(s : String) -> cases.C"));
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

		@:using(cases.Issue10106.EnExtension)
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
	function testEnum() {
		eq(true, hasField(fields(pos(1)), "fromS", "(s : String) -> cases.En"));
	}
}
