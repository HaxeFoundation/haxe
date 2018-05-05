package io;

import utest.Assert;
import sys.io.File;
import sys.FileSystem;

class TestFile {
	public function new() { }

	public function testCopyOverwrite() {
		var fileA = "temp/a.txt";
		var fileB = "temp/b.txt";
		File.saveContent(fileA, "a");
		File.saveContent(fileB, "b");

		Assert.equals("b", File.getContent(fileB));
		File.copy(fileA, fileB);
		Assert.equals("a", File.getContent(fileB));

		// cleanup
		FileSystem.deleteFile(fileA);
		FileSystem.deleteFile(fileB);
	}
}
