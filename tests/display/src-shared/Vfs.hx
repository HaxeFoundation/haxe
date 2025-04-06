package;

import haxe.io.Path;
import sys.FileSystem;
import sys.io.File;

using DateTools;

class Vfs {
	var physicalPath:String;

	public function new(physicalPath:String) {
		this.physicalPath = physicalPath;
		if (!FileSystem.exists(physicalPath)) {
			FileSystem.createDirectory(physicalPath);
		}
	}

	public function overwriteContent(path:String, content:String) {
		var path = getPhysicalPath(path).toString();
		if (!FileSystem.exists(path)) {
			throw 'Cannot overwrite content for $path: file does not exist';
		}
		File.saveContent(path, content);
	}

	public function putContent(path:String, content:String) {
		var path = getPhysicalPath(path);
		FileSystem.createDirectory(path.dir);
		File.saveContent(path.toString(), content);
	}

	public function getContent(path:String):String {
		var path = getPhysicalPath(path);
		FileSystem.createDirectory(path.dir);
		return File.getContent(path.toString());
	}

	public function close() {
		removeDir(physicalPath);
	}

	public function getPhysicalPath(path:String) {
		return new Path(Path.join([physicalPath, path]));
	}

	static public function removeDir(dir:String):Void {
		if (FileSystem.exists(dir)) {
			for (item in FileSystem.readDirectory(dir)) {
				item = haxe.io.Path.join([dir, item]);
				if (FileSystem.isDirectory(item)) {
					removeDir(item);
				} else {
					FileSystem.deleteFile(item);
				}
			}
			FileSystem.deleteDirectory(dir);
		}
	}
}
