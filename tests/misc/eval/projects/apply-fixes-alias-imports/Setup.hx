import sys.FileSystem;
import sys.io.File;

class Setup {
	static function run() {
		copy("cases", "bin");
	}

	static function copy(from:String, to:String) {
		FileSystem.createDirectory(to);
		for (name in FileSystem.readDirectory(from)) {
			var path = '$from/$name';
			if (FileSystem.isDirectory(path))
				copy(path, '$to/$name');
			else if (StringTools.endsWith(name, ".txt"))
				File.saveContent('$to/${name.substr(0, name.length - 4)}', File.getContent(path));
		}
	}
}
