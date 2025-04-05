import sys.FileSystem;

function exit(code:Int) {
	FileSystem.deleteFile("main.js");
	Sys.exit(code);
}

function exitWithError(msg:String, code:Int = 1) {
	FileSystem.deleteFile("main.js");
	Sys.stderr().writeString(msg + "\n");
	Sys.exit(code);
}
