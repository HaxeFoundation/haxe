package test;

import haxe.io.Bytes;
import asys.FileSystem as NewFS;
import asys.io.File as NewFile;
import sys.FileSystem as OldFS;
import sys.io.File as OldFile;

class TestFile extends Test {
	/**
		Tests read functions.
	**/
	function testRead() {
		// ASCII
		var file = NewFS.open("resources-ro/hello.txt");
		var buffer = Bytes.alloc(5);

		eq(file.readBuffer(buffer, 0, 5, 0).bytesRead, 5);
		beq(buffer, Bytes.ofString("hello"));

		eq(file.readBuffer(buffer, 0, 5, 6).buffer, buffer);
		beq(buffer, Bytes.ofString("world"));

		exc(() -> file.readBuffer(buffer, 0, 6, 0));
		exc(() -> file.readBuffer(buffer, -1, 5, 0));
		exc(() -> file.readBuffer(buffer, 0, 0, 0));
		exc(() -> file.readBuffer(buffer, 0, 0, -1));

		buffer = Bytes.alloc(15);
		eq(file.readBuffer(buffer, 0, 5, 0).bytesRead, 5);
		eq(file.readBuffer(buffer, 5, 5, 0).bytesRead, 5);
		eq(file.readBuffer(buffer, 10, 5, 0).bytesRead, 5);
		beq(buffer, Bytes.ofString("hellohellohello"));

		file.close();

		// binary (+ invalid UTF-8)
		var file = NewFS.open("resources-ro/binary.bin");
		var buffer = Bytes.alloc(TestConstants.binaryBytes.length);
		eq(file.readBuffer(buffer, 0, buffer.length, 0).bytesRead, buffer.length);
		beq(buffer, TestConstants.binaryBytes);
		file.close();

		// readFile
		var file = NewFS.open("resources-ro/hello.txt");
		beq(file.readFile(), TestConstants.helloBytes);
		file.close();
	}

	/**
		Tests write functions.
	**/
	function testWrite() {
		var file = NewFS.open("resources-rw/hello.txt", "w");
		var buffer = Bytes.ofString("hello");
		eq(file.writeBuffer(buffer, 0, 5, 0).bytesWritten, 5);
		file.close();

		beq(OldFile.getBytes("resources-rw/hello.txt"), buffer);

		var file = NewFS.open("resources-rw/unicode.txt", "w");
		var buffer = TestConstants.helloBytes;
		eq(file.writeBuffer(buffer, 0, buffer.length, 0).bytesWritten, buffer.length);
		file.close();

		beq(OldFile.getBytes("resources-rw/unicode.txt"), buffer);

		var file = NewFS.open("resources-rw/unicode2.txt", "w");
		eq(file.writeString(TestConstants.helloString, 0).bytesWritten, TestConstants.helloBytes.length);
		file.close();

		beq(OldFile.getBytes("resources-rw/unicode2.txt"), TestConstants.helloBytes);

		// cleanup
		OldFS.deleteFile("resources-rw/hello.txt");
		OldFS.deleteFile("resources-rw/unicode.txt");
		OldFS.deleteFile("resources-rw/unicode2.txt");
	}
}
