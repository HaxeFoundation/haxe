package unit.issues;

class Issue5418 extends unit.Test {
	var TEST_FILE = "issue5418_test.txt";

	#if (sys || nodejs)
	function testOldUsage() {
		var testContent = "line1\nline2\n";
		var expectedLines = ["line1", "line2"];

		sys.io.File.saveContent(TEST_FILE, testContent);

		var is = sys.io.File.read(TEST_FILE, false);

		var lines = [];
		var line;
		try {
			while (true) {
				line = is.readLine();
				if (line != null) lines.push(line); // <- now needs to test for null!
			}
		} catch (e:haxe.io.Eof) {}

		t(is.eof());

		var expected = expectedLines;
		aeq(expected, lines);

		// seek should also reset eof
		is.seek(0, sys.io.FileSeek.SeekBegin);
		aeq([expectedLines[0]], [is.readLine()]);

		is.close();
	}

	function testNewUsage() {
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

		// seek should also reset eof
		is.seek(0, sys.io.FileSeek.SeekBegin);
		aeq([expectedLines[0]], [is.readLine()]);

		is.close();
	}

	function testEmpty() {
		var testContent = "";
		var expectedLines = [];

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

	function testCRLF() {
		var testContent = "\r\n";
		var expectedLines = [""];

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

		// seek should also reset eof
		is.seek(0, sys.io.FileSeek.SeekBegin);
		aeq([expectedLines[0]], [is.readLine()]);

		is.close();
	}
	#end
}
