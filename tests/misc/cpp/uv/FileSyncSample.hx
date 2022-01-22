import haxe.Constraints.Function;
import haxe.io.Bytes;
import cpp.uv.UVError;
import haxe.PosInfos;
import cpp.uv.Misc;
import cpp.uv.UVException;
import sys.thread.Thread;
import cpp.uv.File;
import cpp.uv.FileSync;
import FileSample.Actions;

class FileSyncSample extends UVSample {
	public function run() {
		var actions:Actions = [
			createWriteSyncReadUnlink,
			mkdirRenameRmdir,
			mkdtempRmdir,
			mkstempUnlink,
			statFStat,
			statFs,
			truncate,
			copyFile,
			// sendFile, // Throws EBADF and idk why
			access,
			chmod,
			utime,
			linkSymlinkReadLinkRealPath,
			chown,
		];
		actions.next();
	}

	function createFile(path:String, content:Bytes, ?pos:PosInfos) {
		try {
			var file = FileSync.open(path, [CREAT(420),TRUNC,WRONLY]);
			file.sync.write(content, 0, content.length, 0);
			file.sync.close();
		} catch(e:UVException) {
			throw new UVException(e.error, pos.fileName + ':' + pos.lineNumber + ': ' + e.error.toString(), e);
		}
	}

	function readFile(path:String):Bytes {
		var file = FileSync.open(path, [RDONLY]);
		var buf = Bytes.alloc(10240);
		var bytesRead = file.sync.read(buf, 0, 10240, 0);
		file.sync.close();
		return buf.sub(0, bytesRead);
	}

	function deleteFiles(files:Array<String>) {
		for(path in files)
			FileSync.unlink(path);
	}

	function createWriteSyncReadUnlink(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file';
		print('Creating $path for writing...');
		var file = FileSync.open(path, [CREAT(420), WRONLY]);
		print('Writing...');
		var data = Bytes.ofString('Hello, world!');
		var bytesWritten = file.sync.write(data, 0, data.length, 0);
		print('$bytesWritten bytes written: $data');
		print('fsync...');
		file.sync.fsync();
		print('fdatasync...');
		file.sync.fdataSync();
		file.sync.close();
		print('closed $path');
		readUnlink(path, actions);
	}

	function readUnlink(path:String, actions:Actions) {
		print('Opening $path for reading...');
		var file = FileSync.open(path, [RDONLY]);
		print('Reading...');
		var buf = Bytes.alloc(1024);
		var bytesRead = file.sync.read(buf, 0, 1024, 0);
		print('$bytesRead bytes read: ' + buf.toString());
		file.sync.close();
		print('closed $path');
		unlink(path, actions);
	}

	function unlink(path:String, actions:Actions) {
		print('Unlinking $path...');
		FileSync.unlink(path);
		actions.next();
	}

	function mkdirRenameRmdir(actions:Actions) {
		var path = Misc.tmpDir() + '/test-dir';
		var newPath = Misc.tmpDir() + '/test-dir2';
		print('Creating directory $path...');
		FileSync.mkdir(path, 511);
		print('Renaming $path to $newPath...');
		FileSync.rename(path, newPath);
		print('Removing directory $newPath...');
		FileSync.rmdir(newPath);
		print('Done');
		actions.next();
	}

	function mkdtempRmdir(actions:Actions) {
		var tpl = Misc.tmpDir() + '/test-dir-XXXXXX';
		print('Creating temp directory with tpl $tpl...');
		var path = FileSync.mkdtemp(tpl);
		print('Removing directory $path...');
		FileSync.rmdir(path);
		print('Done');
		actions.next();
	}

	function mkstempUnlink(actions:Actions) {
		var tpl = Misc.tmpDir() + '/test-file-XXXXXX';
		print('Creating temp file with tpl $tpl...');
		var tmp = FileSync.mkstemp(tpl);
		print('Closing ${tmp.path}...');
		tmp.file.sync.close();
		print('Unlinking ${tmp.path}...');
		FileSync.unlink(tmp.path);
		print('Done');
		actions.next();
	}

	function statFStat(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file';
		print('fstat on $path...');
		var file = FileSync.open(path, [CREAT(420)]);
		var fstat = file.sync.fstat();
		print('got fstat: $fstat');
		file.sync.close();
		print('stat on $path');
		var stat = FileSync.stat(path);
		print('got stat: $stat');
		// TODO: jit error on I64 == I64
		// var ok = stat.dev == fstat.dev;
		// 	&& stat.mode == fstat.mode
		// 	&& stat.nlink == fstat.nlink
		// 	&& stat.uid == fstat.uid
		// 	&& stat.gid == fstat.gid
		// 	&& stat.rdev == fstat.rdev
		// 	&& stat.ino == fstat.ino
		// 	&& stat.size == fstat.size
		// 	&& stat.blksize == fstat.blksize
		// 	&& stat.blocks == fstat.blocks
		// 	&& stat.flags == fstat.flags
		// 	&& stat.gen == fstat.gen;
		// print('fstat equals stat: $ok');
		deleteFiles([path]);
		print('Done');
		actions.next();
	}

	function statFs(actions:Actions) {
		print('statfs on .');
		var stat = FileSync.statFs('.');
		print('got statfs: $stat');
		print('Done');
		actions.next();
	}

	function truncate(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file-truncate';
		var content = '1234567890';
		print('Writing content for truncation at $path: $content');
		createFile(path, Bytes.ofString(content));
		var file = FileSync.open(path, [WRONLY]);
		print('truncating at 5...');
		file.sync.ftruncate(5);
		file.sync.close();
		var data = readFile(path);
		print('Content after truncation (length=${data.length}): $data');
		deleteFiles([path]);
		print('Done');
		actions.next();
	}

	function copyFile(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file-copy';
		var newPath = '$path-copy';
		createFile(path, Bytes.ofString('123'));
		print('Copy $path to $newPath');
		FileSync.copyFile(path, newPath, [EXCL]);
		deleteFiles([path, newPath]);
		print('Done');
		actions.next();
	}

	function sendFile(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file-send';
		var newPath = '$path-copy';
		createFile(path, Bytes.ofString('12345678'));
		var src = FileSync.open(path, [RDONLY]);
		var dst = FileSync.open(newPath, [CREAT(420), WRONLY]);
		print('sendFile from $path to $newPath...');
		var outOffset = src.sync.sendFile(dst, 0, 20);
		print('sendfile stopped at $outOffset');
		src.sync.close();
		dst.sync.close();
		deleteFiles([path, newPath]);
		print('Done');
		actions.next();
	}

	function access(actions:Actions) {
		var path = Misc.tmpDir();
		print('Checking write permissions on $path...');
		FileSync.access(path, [W_OK]);
		print('Done');
		actions.next();
	}

	function chmod(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file-chmod';
		createFile(path, Bytes.ofString('123'));
		print('chmod on $path...');
		FileSync.chmod(path, 420);
		deleteFiles([path]);
		print('Done');
		actions.next();
	}

	function utime(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file-utime';
		createFile(path, Bytes.ofString('123'));
		print('utime on $path...');
		FileSync.utime(path, Date.now().getTime(), Date.now().getTime());
		deleteFiles([path]);
		print('Done');
		actions.next();
	}

	function linkSymlinkReadLinkRealPath(actions:Actions) {
		var path = Misc.tmpDir() + '/test-file-l';
		var newPath = Misc.tmpDir() + '/test-file-link';
		createFile(path, Bytes.ofString('123'));
		print('link $path to $newPath...');
		FileSync.link(path, newPath);
		deleteFiles([newPath]);
		print('symlink $path to $newPath...');
		FileSync.symlink(path, newPath, [SYMLINK_JUNCTION]);
		print('readlink at $newPath...');
		var target = FileSync.readLink(newPath);
		print('Link content: $target');
		var real = FileSync.readLink(newPath);
		print('Real path of $newPath: $real');
		deleteFiles([path, newPath]);
		print('Done');
		actions.next();
	}

	function chown(actions:Actions) {
		if(Sys.systemName() == 'Windows') {
			actions.next();
			return;
		}

		var path = Misc.tmpDir() + '/test-file-chown';
		createFile(path, Bytes.ofString(''));
		print('chown on $path...');
		FileSync.chown(path, -1, -1);
		deleteFiles([path]);
		print('Done');
		actions.next();
	}
}