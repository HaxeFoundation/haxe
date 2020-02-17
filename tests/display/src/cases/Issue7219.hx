package cases;

class Issue7219 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				if ([].{-1-}
			}
		}
	**/
	function testIf() {
		eq(true, fields(pos(1)).length > 0);
	}

	/**
		class Main {
			static function main() {
				for ([].{-1-}
			}
		}
	**/
	function testFor() {
		eq(true, fields(pos(1)).length > 0);
	}

	/**
		class Main {
			static function main() {
				while ([].{-1-}
			}
		}
	**/
	function testWhile() {
		eq(true, fields(pos(1)).length > 0);
	}

	/**
		class Main {
			static function main() {
				do [].{-1-}
			}
		}
	**/
	function testDoWhile1() {
		eq(true, fields(pos(1)).length > 0);
	}

	/**
		class Main {
			static function main() {
				do {

				} while ([].{-1-}
			}
		}
	**/
	function testDoWhile2() {
		eq(true, fields(pos(1)).length > 0);
	}
}
