import sys.FileSystem;

function deleteDirectory(path:String) {
	if (!FileSystem.isDirectory(path))
		return FileSystem.deleteFile(path);

	for (item in FileSystem.readDirectory(path))
		deleteDirectory('$path/$item');

	FileSystem.deleteDirectory(path);
}

function main() {
	final json = haxe.Json.parse(sys.io.File.getContent("out/hlc.json"));

	final defines:haxe.DynamicAccess<String> = json.defines;

	final success = Lambda.foreach(defines.keys(), function(define:String) {
		if (!StringTools.contains(define, "-"))
			return true;

		Sys.stderr().writeString("Generated `hlc.json` contains raw version of define flag: " + define + "\n");
		return false;
	});

	deleteDirectory("out");
	Sys.exit(if (success) 0 else 1);
}
