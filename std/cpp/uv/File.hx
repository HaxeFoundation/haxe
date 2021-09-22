/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package cpp.uv;

import haxe.io.Bytes;

using cpp.uv.UV;

@:structInit
class FileStat {
	var dev:UInt64;
	var mode:UInt64;
	var nlink:UInt64;
	var uid:UInt64;
	var gid:UInt64;
	var rdev:UInt64;
	var ino:UInt64;
	var size:UInt64;
	var blksize:UInt64;
	var blocks:UInt64;
	var flags:UInt64;
	var gen:UInt64;
	var atim:FileTimeSpec;
	var mtim:FileTimeSpec;
	var ctim:FileTimeSpec;
	var birthtim:FileTimeSpec;

	public function toString():String {
		return '{dev:$dev, mode:$mode, nlink:$nlink, uid:$uid, gid:$gid, rdev:$rdev, ino:$ino, size:$size, blksize:$blksize, blocks:$blocks, flags:$flags, gen:$gen, atim:$atim, mtim:$mtim, ctim:$ctim, birthtim:$birthtim}';
	}
}

@:structInit
class FileTimeSpec {
	var sec:Int64;
	var nsec:Int64;

	public function toString():String {
		return '{sec:$sec, nsec:$nsec}';
	}
}

/**
	Filesystem operations.

	@see http://docs.libuv.org/en/v1.x/fs.html
**/
@:headerCode('#include "uv.h"')
abstract File(UvFile) {
	static public final stdin:File = new File(new UvFile(0));
	static public final stdout:File = new File(new UvFile(1));
	static public final stderr:File = new File(new UvFile(2));

	@:allow(cpp.uv)
	var uv(get,never):UvFile;
	inline function get_uv():UvFile
		return this;

	@:allow(cpp.uv)
	inline function new(uv:UvFile)
		this = uv;

	static function uvTimespecToHx(times:RawConstPointer<UvTimespecT>):FileTimeSpec {
		var ptr = ConstPointer.fromRaw(times);
		return {
			sec: ptr.value.tv_sec,
			nsec: ptr.value.tv_nsec,
		}
	}

	@:allow(cpp.uv)
	static function uvStatToHx(stat:RawConstPointer<UvStatT>):FileStat {
		var ptr = ConstPointer.fromRaw(stat);
		return {
			dev: ptr.value.st_dev,
			mode: ptr.value.st_mode,
			nlink: ptr.value.st_nlink,
			uid: ptr.value.st_uid,
			gid: ptr.value.st_gid,
			rdev: ptr.value.st_rdev,
			ino: ptr.value.st_ino,
			size: ptr.value.st_size,
			blksize: ptr.value.st_blksize,
			blocks: ptr.value.st_blocks,
			flags: ptr.value.st_flags,
			gen: ptr.value.st_gen,
			atim:uvTimespecToHx(RawConstPointer.addressOf(ptr.value.st_atim)),
			mtim:uvTimespecToHx(RawConstPointer.addressOf(ptr.value.st_mtim)),
			ctim:uvTimespecToHx(RawConstPointer.addressOf(ptr.value.st_ctim)),
			birthtim:uvTimespecToHx(RawConstPointer.addressOf(ptr.value.st_birthtim)),
		}
	}
}