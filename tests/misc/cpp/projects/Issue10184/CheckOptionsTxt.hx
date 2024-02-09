import sys.FileSystem;

function deleteDirectory(path:String) {
	if (!FileSystem.isDirectory(path))
		return FileSystem.deleteFile(path);

	for (item in FileSystem.readDirectory(path))
		deleteDirectory('$path/$item');

	FileSystem.deleteDirectory(path);
}

function main() {
	final definePairs = sys.io.File.getContent("out/Options.txt").split("\n");

	for (definePair in definePairs)
		for (index in 0...definePair.length) {
			final char = definePair.charAt(index);
			if (char == "=") break;
			if (char == "-"){
				deleteDirectory("out");
				Sys.stderr().writeString("Generated `Options.txt` contains raw version of define flag: " + definePair + "\n");
				Sys.exit(1);
			}
		}
	deleteDirectory("out");
	Sys.exit(0);
}
