import sys.FileSystem;

function main() {
	Sys.setCwd("./project");
	FileSystem.createDirectory("./out");
	Sys.command("javac", ["-d", "out", "test/Listeners.java", "-g"]);
	Sys.setCwd("./out");
	Sys.command("jar", ["cf", "test.jar",
		"test/Listeners.class",
		"test/Listeners$OnClick.class",
		"test/Listeners$WithToString.class",
		"test/Listeners$AbstractEqualsPlusOne.class",
		"test/Listeners$WithDefaults.class",
		"test/Listeners$NotSam.class",
		"test/Listeners$StringMaker.class",
		"test/Listeners$Unused.class",
		"test/Listeners$UnaryStringFn.class",
		"test/Listeners$ArgOnly.class",
		"test/Listeners$CtorOnly.class",
		"test/Listeners$CtorSam.class"]);
}
