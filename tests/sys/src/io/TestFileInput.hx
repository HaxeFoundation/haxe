package io;

import utest.Assert;
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
class TestFileInput {

	private var path = 'temp/testcase-test-file.txt';

	public function new() { }

	public function setup() {
		File.saveContent(path, "test\n1234");
	}

	public function tearDown() {
		FileSystem.deleteFile(path);
	}

	public function testRead() {
		var file : FileInput = File.read(path);
		Assert.equals(0, file.tell());
		Assert.equals(116, file.readByte());
		Assert.equals(1, file.tell());
		Assert.equals(101, file.readByte());
		Assert.equals(2, file.tell());
		Assert.equals(115, file.readByte());
		Assert.equals(3, file.tell());
		Assert.equals(116, file.readByte());
		Assert.equals(4, file.tell());
		file.close();
	}

	public function testReadBytes() {
		var file : FileInput = File.read(path);
		var bytes : Bytes = Bytes.alloc (9);
		var count = file.readBytes(bytes, 0, 9);
		Assert.equals(9, count);
		Assert.equals(116, bytes.get(0));
		file.close();
	}

	public function testSeekBeginCur() {
		var file : FileInput = File.read(path);
		Assert.equals(116, file.readByte());
		Assert.equals(101, file.readByte());
		Assert.equals(115, file.readByte());
		Assert.equals(116, file.readByte());

		file.seek(-4, FileSeek.SeekCur);
		Assert.equals(0, file.tell());
		Assert.equals(116, file.readByte());
		Assert.equals(1, file.tell());
		file.close();
	}

	public function testSeekBeginEnd() {
		var file : FileInput = File.read(path);
		Assert.equals(116, file.readByte());
		Assert.equals(101, file.readByte());
		Assert.equals(115, file.readByte());
		Assert.equals(116, file.readByte());

		file.seek(-9, FileSeek.SeekEnd);
		Assert.equals(0, file.tell());
		Assert.equals(116, file.readByte());
		Assert.equals(1, file.tell());
		file.close();
	}

	public function testSeekBegin() {
		var file : FileInput = File.read(path);
		Assert.equals(116, file.readByte());
		Assert.equals(101, file.readByte());
		Assert.equals(115, file.readByte());
		Assert.equals(116, file.readByte());

		file.seek(0, FileSeek.SeekBegin);
		Assert.equals(0, file.tell());
		Assert.equals(116, file.readByte());
		Assert.equals(1, file.tell());
		file.close();
	}

	public function testSeekPosBegin() {
		var file : FileInput = File.read(path);
		Assert.equals(116, file.readByte());
		Assert.equals(101, file.readByte());
		Assert.equals(115, file.readByte());
		Assert.equals(116, file.readByte());

		file.seek(1, FileSeek.SeekBegin);
		Assert.equals(1, file.tell());
		Assert.equals(101, file.readByte());
		Assert.equals(2, file.tell());
		file.close();
	}

	public function testSeekPosBeginMulti() {
		var file : FileInput = File.read(path);
		Assert.equals(116, file.readByte());
		Assert.equals(101, file.readByte());
		Assert.equals(115, file.readByte());
		Assert.equals(116, file.readByte());

		file.seek(1, FileSeek.SeekBegin);
		Assert.equals(1, file.tell());
		Assert.equals(101, file.readByte());
		Assert.equals(2, file.tell());
		file.seek(3, FileSeek.SeekBegin);
		Assert.equals(3, file.tell());
		Assert.equals(116, file.readByte());
		Assert.equals(4, file.tell());
		file.close();
	}

	public function testSeekEnd() {
		var file : FileInput = File.read(path);
		Assert.equals(116, file.readByte());
		Assert.equals(101, file.readByte());
		Assert.equals(115, file.readByte());
		Assert.equals(116, file.readByte());

		file.seek(-1, FileSeek.SeekEnd);
		Assert.equals(8, file.tell());
		Assert.equals(52, file.readByte());
		Assert.equals(9, file.tell());
		file.close();
	}

	public function testSeekEofLast() {
		var file : FileInput = File.read(path);
		Assert.equals(116, file.readByte());
		Assert.equals(101, file.readByte());
		Assert.equals(115, file.readByte());
		Assert.equals(116, file.readByte());

		file.seek(0, FileSeek.SeekEnd);
		Assert.equals(9, file.tell());
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
		Assert.equals(116, file.readByte());
		Assert.equals(101, file.readByte());
		Assert.equals(115, file.readByte());
		Assert.equals(116, file.readByte());

		file.seek(0, FileSeek.SeekEnd);
		Assert.equals(9, file.tell());
		Assert.isFalse(file.eof());
		try {
			file.readByte();
			Assert.isTrue(false);
		} catch(e : haxe.io.Eof) {
			Assert.isTrue(true);
		}
		Assert.isTrue(file.eof());
		Assert.equals(9, file.tell());
		file.seek(-1, FileSeek.SeekCur);
		Assert.equals(8, file.tell());
		Assert.equals(52, file.readByte());
		Assert.equals(9, file.tell());
		Assert.isFalse(file.eof());
		try {
			file.readByte();
			Assert.isTrue(false);
		} catch(e : haxe.io.Eof) {
			Assert.isTrue(true);
		}
		Assert.isTrue(file.eof());
		Assert.equals(9, file.tell());
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
}

