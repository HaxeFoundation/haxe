import sys.FileSystem;
import sys.io.File;

class Check {
	static var failed = false;

	static function main() {
		check("cases", "bin");
		if (failed)
			Sys.exit(1);
	}

	static function check(from:String, to:String) {
		for (name in FileSystem.readDirectory(from)) {
			var path = '$from/$name';
			if (FileSystem.isDirectory(path)) {
				check(path, '$to/$name');
			} else if (StringTools.endsWith(name, ".expected")) {
				var target = '$to/${name.substr(0, name.length - 9)}';
				var actual = File.getContent(target).split("\r\n").join("\n");
				if (actual != File.getContent(path).split("\r\n").join("\n")) {
					Sys.println(target);
					Sys.println(actual);
					failed = true;
				}
			}
		}
	}
}
