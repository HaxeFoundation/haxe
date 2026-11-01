package cases.display.issues;

class Issue3298 extends DisplayTestCase {
	/**
		class Main { static function main() { {-1-}h } }
	**/
	function testToplevelCompletion(_) {
		var items = toplevel(1);
		Assert.isTrue(items.length > 0);
	}
}
