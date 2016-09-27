package io;

import sys.io.FileInput;
import sys.FileSystem;
import sys.io.File;
import sys.io.FileSeek;

/**
 * Class TestFileInput
 *
 * @author        Maximilian Ruta <mr@xtain.net>
 */
class TestFileInput extends haxe.unit.TestCase {

	private var path = 'temp/testcase-test-file.txt';

	override public function setup() {
		File.saveContent(path, "test\n1234");
	}

	override public function tearDown() {
		FileSystem.deleteFile(path);
	}

	public function testRead() {
		var file : FileInput = File.read(path);
		assertEquals(0, file.tell());
		assertEquals(116, file.readByte());
		assertEquals(1, file.tell());
		assertEquals(101, file.readByte());
		assertEquals(2, file.tell());
		assertEquals(115, file.readByte());
		assertEquals(3, file.tell());
		assertEquals(116, file.readByte());
		assertEquals(4, file.tell());
		file.close();
	}

	public function testSeekBeginCur() {
		var file : FileInput = File.read(path);
		assertEquals(116, file.readByte());
		assertEquals(101, file.readByte());
		assertEquals(115, file.readByte());
		assertEquals(116, file.readByte());

		file.seek(-4, FileSeek.SeekCur);
		assertEquals(0, file.tell());
		assertEquals(116, file.readByte());
		assertEquals(1, file.tell());
		file.close();
	}

	public function testSeekBeginEnd() {
		var file : FileInput = File.read(path);
		assertEquals(116, file.readByte());
		assertEquals(101, file.readByte());
		assertEquals(115, file.readByte());
		assertEquals(116, file.readByte());

		file.seek(-9, FileSeek.SeekEnd);
		assertEquals(0, file.tell());
		assertEquals(116, file.readByte());
		assertEquals(1, file.tell());
		file.close();
	}

	public function testSeekBegin() {
		var file : FileInput = File.read(path);
		assertEquals(116, file.readByte());
		assertEquals(101, file.readByte());
		assertEquals(115, file.readByte());
		assertEquals(116, file.readByte());

		file.seek(0, FileSeek.SeekBegin);
		assertEquals(0, file.tell());
		assertEquals(116, file.readByte());
		assertEquals(1, file.tell());
		file.close();
	}

	public function testSeekPosBegin() {
		var file : FileInput = File.read(path);
		assertEquals(116, file.readByte());
		assertEquals(101, file.readByte());
		assertEquals(115, file.readByte());
		assertEquals(116, file.readByte());

		file.seek(1, FileSeek.SeekBegin);
		assertEquals(1, file.tell());
		assertEquals(101, file.readByte());
		assertEquals(2, file.tell());
		file.close();
	}

	public function testSeekPosBeginMulti() {
		var file : FileInput = File.read(path);
		assertEquals(116, file.readByte());
		assertEquals(101, file.readByte());
		assertEquals(115, file.readByte());
		assertEquals(116, file.readByte());

		file.seek(1, FileSeek.SeekBegin);
		assertEquals(1, file.tell());
		assertEquals(101, file.readByte());
		assertEquals(2, file.tell());
		file.seek(3, FileSeek.SeekBegin);
		assertEquals(3, file.tell());
		assertEquals(116, file.readByte());
		assertEquals(4, file.tell());
		file.close();
	}

	public function testSeekEnd() {
		var file : FileInput = File.read(path);
		assertEquals(116, file.readByte());
		assertEquals(101, file.readByte());
		assertEquals(115, file.readByte());
		assertEquals(116, file.readByte());

		file.seek(-1, FileSeek.SeekEnd);
		assertEquals(8, file.tell());
		assertEquals(52, file.readByte());
		assertEquals(9, file.tell());
		file.close();
	}

	public function testSeekEofLast() {
		var file : FileInput = File.read(path);
		assertEquals(116, file.readByte());
		assertEquals(101, file.readByte());
		assertEquals(115, file.readByte());
		assertEquals(116, file.readByte());

		file.seek(0, FileSeek.SeekEnd);
		assertEquals(9, file.tell());
		assertFalse(file.eof());
		try {
			file.readByte();
			assertTrue(false);
		} catch(e : haxe.io.Eof) {
			assertTrue(true);
		}
		assertTrue(file.eof());
		file.close();
	}

	public function testSeekEof() {
		var file : FileInput = File.read(path);
		assertEquals(116, file.readByte());
		assertEquals(101, file.readByte());
		assertEquals(115, file.readByte());
		assertEquals(116, file.readByte());

		file.seek(0, FileSeek.SeekEnd);
		assertEquals(9, file.tell());
		assertFalse(file.eof());
		try {
			file.readByte();
			assertTrue(false);
		} catch(e : haxe.io.Eof) {
			assertTrue(true);
		}
		assertTrue(file.eof());
		assertEquals(9, file.tell());
		file.seek(-1, FileSeek.SeekCur);
		assertEquals(8, file.tell());
		assertEquals(52, file.readByte());
		assertEquals(9, file.tell());
		assertFalse(file.eof());
		try {
			file.readByte();
			assertTrue(false);
		} catch(e : haxe.io.Eof) {
			assertTrue(true);
		}
		assertTrue(file.eof());
		assertEquals(9, file.tell());
		try {
			file.readByte();
			assertTrue(false);
		} catch(e : haxe.io.Eof) {
			assertTrue(true);
		}

		file.seek(5, FileSeek.SeekEnd);
		assertFalse(file.eof());
		try {
			file.readByte();
			assertTrue(false);
		} catch(e : haxe.io.Eof) {
			assertTrue(true);
		}
		assertTrue(file.eof());
		file.seek(1, FileSeek.SeekEnd);
		assertFalse(file.eof());
		try {
			file.readByte();
			assertTrue(false);
		} catch(e : haxe.io.Eof) {
			assertTrue(true);
		}
		assertTrue(file.eof());
		try {
			file.readByte();
			assertTrue(false);
		} catch(e : haxe.io.Eof) {
			assertTrue(true);
		}
		file.close();
	}
}

