package unit.issues;

import unit.issues.misc.Issue11385LibraryData;

class Issue11385 extends Test {
	function test() {
		eq("Hello World", Issue11385LibraryData.isCommitHash());
	}
}
