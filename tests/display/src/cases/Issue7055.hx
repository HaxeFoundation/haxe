package cases;

class Issue7055 extends DisplayTestCase {
	/**
		enum TestEnum {
			Some;
			Random;
			Enum;
			Constructors;
			For;
			Testing;
		}

		class Main {
			static function main() {
				switch ((null:TestEnum)) {
					case {-1-}
				}
			}
		}
	**/
	function test() {
		var results = toplevel(pos(1));
		var i = 0;
		function nextIs(name, ?pos) {
			eq(true, isToplevel(results[i++], name), pos);
		}
		nextIs("Some");
		nextIs("Random");
		nextIs("Enum");
		nextIs("Constructors");
		nextIs("For");
		nextIs("Testing");
	}
}
