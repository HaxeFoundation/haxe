package unit.issues;

class Issue6717 extends unit.Test {
	function test() {
		var space = " ";  // ASCII 0x20
		var hyphen = "-"; // ASCII 0x2D

		t(space.charAt(0) < hyphen.charAt(0));

		// example strings from https://docs.microsoft.com/en-us/dotnet/api/system.globalization.compareoptions
		var unsorted = ["cant", "bill's", "coop", "cannot", "billet", "can't", "con", "bills", "co-op"];
		var expected = ["bill's", "billet", "bills", "can't", "cannot", "cant", "co-op", "con", "coop"];

		var actual = unsorted.copy();
		actual.sort(strcmp);

		aeq(expected, actual);
	}

	static function strcmp(a:String, b:String):Int {
		return a < b ? -1 : a > b ? 1 : 0;
	}
}
