package io;

import sys.io.File;
import sys.FileSystem;

class TestFile extends haxe.unit.TestCase {
	public function testCopyOverwrite() {
		var fileA = "temp/a.txt";
		var fileB = "temp/b.txt";
		File.saveContent(fileA, "a");
		File.saveContent(fileB, "b");

		assertEquals("b", File.getContent(fileB));
		File.copy(fileA, fileB);
		assertEquals("a", File.getContent(fileB));

		// cleanup
		FileSystem.deleteFile(fileA);
		FileSystem.deleteFile(fileB);
	}
}
