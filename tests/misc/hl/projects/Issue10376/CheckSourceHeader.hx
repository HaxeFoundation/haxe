import sys.FileSystem;

final FILE = "out/main.c";

function deleteDirectory(path:String) {
	if (!FileSystem.isDirectory(path))
		return FileSystem.deleteFile(path);

	for (item in FileSystem.readDirectory(path))
		deleteDirectory('$path/$item');

	FileSystem.deleteDirectory(path);
}

function main() {
	final args = Sys.args();

	final code =
		try {
			switch args {
				case [""]:
					checkForEmptySourceHeader(FILE);
				case [expected]:
					checkSourceHeader(FILE, expected);
				case _:
					throw "Incorrect number of arguments to script.";
			}
			0;
		} catch (e){
			Sys.stderr().writeString(e + "\n");
			1;
		}

	deleteDirectory("out");
	Sys.exit(code);
}

function checkForEmptySourceHeader(path:String) {
	final content = getCSourceContent(path);

	if (StringTools.startsWith(content, "// "))
		throw "File has a source header when none was expected: " + content.split("\n")[0];
}

function checkSourceHeader(path:String, expected:String) {
	final content = getCSourceContent(path);

	if (!StringTools.startsWith(content, "// " + expected))
		throw "File source header does not start with expected: // " + expected +
			"\nSource header: " + content.split("\n")[0];
}

function getCSourceContent(path:String) {
	// have to skip the BOM character
	return sys.io.File.getContent(path).substr(1);
}
