package unit.issues;

class Issue5418 extends unit.Test {
	var TEST_FILE = "issue5418_test.txt";

	#if (sys || nodejs)
	function testIssue() {
		var testContent = "line1\nline2\n";
		var expectedLines = ["line1", "line2"];

		sys.io.File.saveContent(TEST_FILE, testContent);

		var is = sys.io.File.read(TEST_FILE, false);

		var lines = [];
		var line;
		while ((line = is.readLine()) != null) {
			lines.push(line);
		}

		t(is.eof());

		var expected = expectedLines;
		aeq(expected, lines);

		is.close();
	}
	#end
}
