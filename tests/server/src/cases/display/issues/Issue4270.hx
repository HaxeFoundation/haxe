package cases.display.issues;

class Issue4270 extends DisplayTestCase {
	/**
		class Main {
		    static function main() {
		        "".{-1-}
		    }
		}
	**/
	function testEmptyStringNoCode(_) {
		var items = fields(1);
		Assert.isFalse(hasField(items, "code"), "Empty string should not have a .code field");
	}

	/**
		class Main {
		    static function main() {
		        "a".{-1-}
		    }
		}
	**/
	function testSingleCharHasCode(_) {
		var items = fields(1);
		Assert.isTrue(hasField(items, "code"), "Single-char string should have a .code field");
	}

	/**
		class Main {
		    static function main() {
		        "ab".{-1-}
		    }
		}
	**/
	function testMultiCharNoCode(_) {
		var items = fields(1);
		Assert.isFalse(hasField(items, "code"), "Multi-char string should not have a .code field");
	}
}
