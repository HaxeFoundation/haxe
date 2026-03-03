package cases.display.issues;

class Issue4662 extends DisplayTestCase {
	/**
		class Main { static function main() { {-1-} } }
	**/
	function testUnderscorePackageNotInCompletion(_) {
		vfs.putContent("_pkg/A.hx", "package _pkg;\nclass A {}");
		var items = toplevel(1);
		for (item in items) {
			switch item.kind {
				case Package:
					Assert.isFalse(item.args.path.pack[0] == "_pkg",
						"Package starting with _ should not appear in completion");
				case _:
			}
		}
	}
}
