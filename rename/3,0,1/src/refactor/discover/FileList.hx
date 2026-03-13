package refactor.discover;

class FileList {
	public final files:Map<String, File> = [];

	var recentlyRenamed:Array<String> = [];

	public function new() {}

	public function addFile(file:File) {
		files.set(file.name, file);
		recentlyRenamed.remove(file.name);
	}

	public function addRecentlyRenamed(file:File) {
		recentlyRenamed.push(file.name);
	}

	public function wasRecentlyRenamed(fileName:String):Bool {
		if (recentlyRenamed.contains(fileName)) {
			recentlyRenamed.remove(fileName);
			return true;
		}
		return false;
	}

	public function getFile(fileName:String):Null<File> {
		return files.get(fileName);
	}

	public function removeFile(fileName:String) {
		files.remove(fileName);
	}

	public function clear() {
		files.clear();
		recentlyRenamed = [];
	}
}
