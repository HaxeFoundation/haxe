import cpp.uv.DirSync;
import cpp.uv.Misc;

class DirSyncSample extends UVSample {
	public function run() {
		var path = Misc.tmpDir();
		var dir = DirSync.open(path);
		var entries = dir.sync.read(3);
		for(i in 0...entries.length)
			print('\t${entries[i]}');
		dir.sync.close();
	}
}