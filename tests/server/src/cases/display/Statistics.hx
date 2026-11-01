package cases.display;

import haxe.display.Display;

class Statistics extends DisplayTestCase {
	/**
		class Main {
		    static function helper() { }

		    static function main() {
		        {-1-}helper();
		        helper();
		    }
		}
	**/
	function testReferenceCounting(_) {
		var args = ["-main", "Main", "--no-output"];
		runHaxe(args);
		final result = runHaxeJson(args, DisplayMethods.Statistics, {file: file});
		Assert.isTrue(result.length > 0, "Statistics should return at least one file entry");
		final fileEntry = result[0];
		Assert.isTrue(fileEntry.statistics.length > 0, "Statistics should contain entries");
		// Find the entry for the 'helper' function - it should have references
		var hasReferences = false;
		for (entry in fileEntry.statistics) {
			if (entry.references != null && entry.references.length > 0) {
				hasReferences = true;
				break;
			}
		}
		Assert.isTrue(hasReferences, "Statistics should contain entries with references");
	}
}
