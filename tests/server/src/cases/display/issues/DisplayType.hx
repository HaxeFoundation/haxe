package cases.display.issues;

class DisplayType extends DisplayTestCase {
	/**
		class Main { static function main() { var a = ""; {-1-}a; } }
	**/
	function testHoverStringVar(_) {
		eq("String", type(1));
	}
}
