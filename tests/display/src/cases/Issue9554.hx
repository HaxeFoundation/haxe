package cases;

class Issue9554 extends DisplayTestCase {
	/**
		using Main;

		class Main {
			static public function main() {
				infer({foo: 12});
			}

			static function infer(a) {
				a.foo = 12;
				a.{-1-}
			}

			static function staticExtension(a:{foo:Int}) {}
		}

	**/
	function testCatch_noTypeHint() {
		var fields = fields(pos(1));
		eq(1, fields.length);
		eq(true, hasField(fields, "foo", "Int", "var"));
	}
}
