final FILE = "main.js";

function main() {
	final args = Sys.args();

	switch args {
		case [""]:
			checkForEmptySourceHeader(FILE);
		case [expected]:
			checkSourceHeader(FILE, expected);
		case _:
			Tools.exitWithError("Incorrect number of arguments to script.");
	}

	Tools.exit(0);
}

function checkForEmptySourceHeader(path:String) {
	final content = getJsSourceContent(path);

	if (StringTools.startsWith(content, "// "))
		Tools.exitWithError("File has a source header when none was expected: " + content.split("\n")[0]);
}

function checkSourceHeader(path:String, expected:String) {
	final content = getJsSourceContent(path);

	if (!StringTools.startsWith(content, "// " + expected))
		Tools.exitWithError("File source header does not start with expected: // " + expected +
			"\nSource header: " + content.split("\n")[0]);
}

function getJsSourceContent(path:String) {
	return sys.io.File.getContent(path);
}
