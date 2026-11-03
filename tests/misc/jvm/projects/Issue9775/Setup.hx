import sys.FileSystem;
import sys.io.File;

// Generates enough jars with long names that the manifest's Class-Path
// line would exceed the 512 byte line limit of the JVM's manifest reader.
final COUNT = 10;

function main() {
	FileSystem.createDirectory("bin/src");
	FileSystem.createDirectory("bin/jars");
	final sources = [];
	for (i in 0...COUNT) {
		FileSystem.createDirectory('bin/src/p$i');
		File.saveContent('bin/src/p$i/Lib.java', 'package p$i; public class Lib { public static int get() { return $i; } }');
		sources.push('bin/src/p$i/Lib.java');
	}
	check(Sys.command("javac", ["-d", "bin/classes"].concat(sources)));
	for (i in 0...COUNT) {
		check(Sys.command("jar", ["cf", 'bin/jars/a-library-with-a-rather-long-name-to-make-the-manifest-class-path-line-very-long-$i.jar', "-C", "bin/classes", 'p$i']));
	}
}

function check(code:Int) {
	if (code != 0) {
		Sys.exit(code);
	}
}