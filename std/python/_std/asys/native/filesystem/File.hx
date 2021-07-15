package asys.native.filesystem;

import python.lib.Os;
import python.Syntax;
import python.Tuple;
import python.lib.io.RawIOBase;
import haxe.Int64;
import haxe.io.Bytes;
import haxe.NoData;
import haxe.exceptions.NotImplementedException;
import haxe.exceptions.NotSupportedException;
import asys.native.system.SystemUser;
import asys.native.system.SystemGroup;
import asys.native.filesystem.FileSystem.pool;

@:coreApi
class File {
	public final path:FilePath;

	final handle:RawIOBase;

	@:allow(asys.native.filesystem.FileSystem)
	function new(handle:RawIOBase, path:FilePath):Void {
		this.path = path;
		this.handle = handle;
	}

	public function write(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		pool.runFor(
			() -> {
				if(length < 0)
					throw new FsException(CustomError('File.write(): negative length'), path);
				if(position < 0)
					throw new FsException(CustomError('File.write(): negative position'), path);
				if(offset < 0 || offset > buffer.length)
					throw new FsException(CustomError('File.write(): offset out of buffer bounds'), path);
				try {
					handle.seek(Int64.toInt(position), SeekSet);
					handle.write(Syntax.code("{0}[{1}:{2}]", buffer.getData(), offset, offset + length));
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function read(position:Int64, buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		pool.runFor(
			() -> {
				if(length < 0)
					throw new FsException(CustomError('File.read(): negative length'), path);
				if(position < 0)
					throw new FsException(CustomError('File.read(): negative position'), path);
				if(offset < 0 || offset > buffer.length)
					throw new FsException(CustomError('File.read(): offset out of buffer bounds'), path);
				if(offset + length > buffer.length)
					length = buffer.length - offset;
				try {
					handle.seek(Int64.toInt(position), SeekSet);
					var b = handle.read(length);
					Syntax.code("{0}.b[{1}:{1}+{2}] = {3}", buffer, offset, b.length, b);
					b.length;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function flush(callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					handle.flush();
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function info(callback:Callback<FileInfo>):Void {
		pool.runFor(
			() -> {
				try {
					Os.stat(handle.fileno());
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function setPermissions(permissions:FilePermissions, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					Os.chmod(handle.fileno(), permissions);
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function setOwner(user:SystemUser, group:SystemGroup, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					if(Os.name == 'posix')
						Os.chown(handle.fileno(), user, group);
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function resize(newSize:Int, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					Os.ftruncate(handle.fileno(), newSize);
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function setTimes(accessTime:Int, modificationTime:Int, callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					Os.utime(handle.fileno(), Tuple2.make(accessTime, modificationTime));
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}

	public function close(callback:Callback<NoData>):Void {
		pool.runFor(
			() -> {
				try {
					handle.close();
					NoData;
				} catch(e) {
					throw new FsException(CustomError(e.toString()), path);
				}
			},
			callback
		);
	}
}