package io;

import utest.Assert;
import haxe.Int64;
import haxe.io.Bytes;
import sys.io.FileInput;
import sys.FileSystem;
import sys.io.File;
import sys.io.FileSeek;

/**
 * Class TestFileInput
 *
 * @author        Maximilian Ruta <mr@xtain.net>
 */
class TestFileInput extends utest.Test {
	static var contentString = "test\n1234сюрприз!"; //that's total of 24 bytes
	static var contentBytes = [116, 101, 115, 116, 10, 49, 50, 51, 52, 209, 129, 209, 142, 209, 128, 208, 191, 209, 128, 208, 184, 208, 183, 33];

	private var path = 'temp/testcase-test-file.txt';

	public function setup() {
		File.saveContent(path, contentString);
	}

	public function teardown() {
		FileSystem.deleteFile(path);
	}

	#if !(cpp || neko || (hl && !(hl_ver >= version("1.12.0")))) // their native seek and tell are 32-bit
	public function testSeekLarge() {
		var file:FileInput = File.read(path);
		#if lua
		// some Lua builds (e.g. Lua 5.1, or MinGW builds on Windows) only have 32-bit file offsets
		var native = @:privateAccess file.f;
		native.seek("set", cast 4294967312.0);
		var nativePos:Null<Float> = native.seek();
		if (nativePos != 4294967312.0) {
			file.close();
			Assert.pass();
			return;
		}
		#end
		var big = Int64.make(1, 16); // 4GB + 16
		file.seek(big, FileSeek.SeekBegin);
		Assert.isTrue(file.tell() == big);
		file.seek(-16, FileSeek.SeekCur);
		Assert.isTrue(file.tell() == Int64.make(1, 0));
		file.seek(0, FileSeek.SeekBegin);
		Assert.equals(contentBytes[0], file.readByte());
		file.close();
	}
	#end

	public function testRead() {
		var file : FileInput = File.read(path);
		for(i in 0...contentBytes.length) {
			Assert.equals(i, Int64.toInt(file.tell()));
			Assert.equals(contentBytes[i], file.readByte());
		}
		file.close();
	}

	public function testReadBytes() {
		var file : FileInput = File.read(path);
		var bytes : Bytes = Bytes.alloc (contentBytes.length);
		var count = file.readBytes(bytes, 0, contentBytes.length);
		Assert.equals(contentBytes.length, count);
		for(i in 0...contentBytes.length) {
			Assert.equals(contentBytes[i], bytes.get(i));
		}
		file.close();
	}

	public function testSeekBeginCur() {
		var file : FileInput = File.read(path);
		for(i in 0...Std.int(contentBytes.length / 2)) {
			Assert.equals(contentBytes[i], file.readByte());
		}

		file.seek(-4, FileSeek.SeekCur);
		var pos = Std.int(contentBytes.length / 2) - 4;
		Assert.equals(pos, Int64.toInt(file.tell()));
		Assert.equals(contentBytes[pos], file.readByte());
		Assert.equals(pos + 1, Int64.toInt(file.tell()));
		file.close();
	}

	public function testSeekBeginEnd() {
		var file : FileInput = File.read(path);
		for(i in 0...Std.int(contentBytes.length / 2)) {
			Assert.equals(contentBytes[i], file.readByte());
		}

		file.seek(-contentBytes.length, FileSeek.SeekEnd);
		Assert.equals(0, Int64.toInt(file.tell()));
		Assert.equals(contentBytes[0], file.readByte());
		Assert.equals(1, Int64.toInt(file.tell()));
		file.close();
	}

	public function testSeekBegin() {
		var file : FileInput = File.read(path);
		for(i in 0...Std.int(contentBytes.length / 2)) {
			Assert.equals(contentBytes[i], file.readByte());
		}

		file.seek(0, FileSeek.SeekBegin);
		Assert.equals(0, Int64.toInt(file.tell()));
		Assert.equals(contentBytes[0], file.readByte());
		Assert.equals(1, Int64.toInt(file.tell()));
		file.close();
	}

	public function testSeekPosBegin() {
		var file : FileInput = File.read(path);
		for(i in 0...Std.int(contentBytes.length / 2)) {
			Assert.equals(contentBytes[i], file.readByte());
		}

		file.seek(1, FileSeek.SeekBegin);
		Assert.equals(1, Int64.toInt(file.tell()));
		Assert.equals(contentBytes[1], file.readByte());
		Assert.equals(2, Int64.toInt(file.tell()));
		file.close();
	}

	public function testSeekPosBeginMulti() {
		var file : FileInput = File.read(path);
		for(i in 0...Std.int(contentBytes.length / 2)) {
			Assert.equals(contentBytes[i], file.readByte());
		}

		file.seek(1, FileSeek.SeekBegin);
		Assert.equals(1, Int64.toInt(file.tell()));
		Assert.equals(contentBytes[1], file.readByte());
		Assert.equals(2, Int64.toInt(file.tell()));
		file.seek(3, FileSeek.SeekBegin);
		Assert.equals(3, Int64.toInt(file.tell()));
		Assert.equals(contentBytes[3], file.readByte());
		Assert.equals(4, Int64.toInt(file.tell()));
		file.close();
	}

	public function testSeekEnd() {
		var file : FileInput = File.read(path);
		for(i in 0...Std.int(contentBytes.length / 2)) {
			Assert.equals(contentBytes[i], file.readByte());
		}

		file.seek(-1, FileSeek.SeekEnd);
		var pos = contentBytes.length - 1;
		Assert.equals(pos, Int64.toInt(file.tell()));
		Assert.equals(contentBytes[pos], file.readByte());
		Assert.equals(pos + 1, Int64.toInt(file.tell()));
		file.close();
	}

	public function testSeekEofLast() {
		var file : FileInput = File.read(path);
		for(i in 0...Std.int(contentBytes.length / 2)) {
			Assert.equals(contentBytes[i], file.readByte());
		}

		file.seek(0, FileSeek.SeekEnd);
		Assert.equals(contentBytes.length, Int64.toInt(file.tell()));
		Assert.isFalse(file.eof());
		try {
			file.readByte();
			Assert.isTrue(false);
		} catch(e : haxe.io.Eof) {
			Assert.isTrue(true);
		}
		Assert.isTrue(file.eof());
		file.close();
	}

	public function testSeekEof() {
		var file : FileInput = File.read(path);
		for(i in 0...Std.int(contentBytes.length / 2)) {
			Assert.equals(contentBytes[i], file.readByte());
		}

		file.seek(0, FileSeek.SeekEnd);
		Assert.equals(contentBytes.length, Int64.toInt(file.tell()));
		Assert.isFalse(file.eof());
		try {
			file.readByte();
			Assert.isTrue(false);
		} catch(e : haxe.io.Eof) {
			Assert.isTrue(true);
		}
		Assert.isTrue(file.eof());
		Assert.equals(contentBytes.length, Int64.toInt(file.tell()));
		file.seek(-1, FileSeek.SeekCur);
		var pos = contentBytes.length - 1;
		Assert.equals(pos, Int64.toInt(file.tell()));
		Assert.equals(contentBytes[pos], file.readByte());
		Assert.equals(pos + 1, Int64.toInt(file.tell()));
		Assert.isFalse(file.eof());
		try {
			file.readByte();
			Assert.isTrue(false);
		} catch(e : haxe.io.Eof) {
			Assert.isTrue(true);
		}
		Assert.isTrue(file.eof());
		Assert.equals(pos + 1, Int64.toInt(file.tell()));
		try {
			file.readByte();
			Assert.isTrue(false);
		} catch(e : haxe.io.Eof) {
			Assert.isTrue(true);
		}

		file.seek(5, FileSeek.SeekEnd);
		Assert.isFalse(file.eof());
		try {
			file.readByte();
			Assert.isTrue(false);
		} catch(e : haxe.io.Eof) {
			Assert.isTrue(true);
		}
		Assert.isTrue(file.eof());
		file.seek(1, FileSeek.SeekEnd);
		Assert.isFalse(file.eof());
		try {
			file.readByte();
			Assert.isTrue(false);
		} catch(e : haxe.io.Eof) {
			Assert.isTrue(true);
		}
		Assert.isTrue(file.eof());
		try {
			file.readByte();
			Assert.isTrue(false);
		} catch(e : haxe.io.Eof) {
			Assert.isTrue(true);
		}
		file.close();
	}

	function testIssue7544() {
		var file = sys.io.File.read(path, true);
		var buf = haxe.io.Bytes.alloc(contentBytes.length);

		function next() {
			try {
				var read = file.readBytes(buf, 0, contentBytes.length);
				return Std.string(read);
			} catch(e:haxe.io.Eof) {
				return Std.string('eof');
			} catch(e:Dynamic) {
				return Std.string(e);
			}
		}

		Assert.equals('24', next());
		next(); // TODO: at this line, some target produce '0', some produce 'eof', do we need to unify?
		Assert.equals('eof', next());
		file.close();
	}
}

