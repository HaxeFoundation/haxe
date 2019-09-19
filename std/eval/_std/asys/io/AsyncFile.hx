package asys.io;

import haxe.NoData;
import haxe.async.*;
import haxe.io.Bytes;
import haxe.io.Encoding;

class AsyncFile {
	extern public function chmod(mode:FilePermissions, callback:Callback<NoData>):Void;

	extern public function chown(uid:Int, gid:Int, callback:Callback<NoData>):Void;

	extern public function close(callback:Callback<NoData>):Void;

	extern public function datasync(callback:Callback<NoData>):Void;

	extern public function readBuffer(buffer:Bytes, offset:Int, length:Int, position:Int, callback:Callback<{bytesRead:Int, buffer:Bytes}>):Void;

	public function readFile(callback:Callback<Bytes>):Void {
		stat((err, stat) -> {
			if (err != null)
				return callback(err, null);
			var buffer = Bytes.alloc(stat.size);
			readBuffer(buffer, 0, buffer.length, 0, (err, res) -> {
				if (err != null)
					return callback(err, null);
				callback(null, buffer);
			});
		});
	}

	extern public function stat(callback:Callback<eval.uv.Stat>):Void;

	extern public function sync(callback:Callback<NoData>):Void;

	extern public function truncate(?len:Int = 0, callback:Callback<NoData>):Void;

	extern function utimes_native(atime:Float, mtime:Float, callback:Callback<NoData>):Void;

	public function utimes(atime:Date, mtime:Date, callback:Callback<NoData>):Void {
		utimes_native(atime.getTime() / 1000, mtime.getTime() / 1000, callback);
	}

	extern public function writeBuffer(buffer:Bytes, offset:Int, length:Int, position:Int, callback:Callback<{bytesWritten:Int, buffer:Bytes}>):Void;

	public function writeString(str:String, ?position:Int, ?encoding:Encoding, callback:Callback<{bytesWritten:Int, buffer:Bytes}>):Void {
		var buffer = Bytes.ofString(str, encoding);
		writeBuffer(buffer, 0, buffer.length, position, callback);
	}
}
