package unit.teststd.sys.io;

class TestFile extends unit.Test {
	public function test() {
		#if (sys || nodejs)
		var filename = '.sys.io.file.testfile';
		if (sys.FileSystem.exists(filename)) sys.FileSystem.deleteFile(filename);
		f(sys.FileSystem.exists(filename));

		// test file write
		var fw = sys.io.File.write(filename);
		t(sys.FileSystem.exists(filename));
		fw.writeString("apple\n");
		fw.close();
		eq(sys.io.File.getContent(filename), "apple\n");

		// overwrite
		var fw = sys.io.File.write(filename);
		fw.writeString("banana\n");
		fw.close();
		eq(sys.io.File.getContent(filename), "banana\n");

		// test file append
		var fa = sys.io.File.append(filename);
		fa.writeString("apple\n");
		fa.close();
		eq(sys.io.File.getContent(filename), "banana\napple\n");

		// test file update
		var fu = sys.io.File.update(filename);
		fu.writeString("cherry\n");
		fu.close();
		eq(sys.io.File.getContent(filename), "cherry\napple\n");
		var fu = sys.io.File.update(filename);
		fu.seek(7, sys.io.FileSeek.SeekBegin);
		fu.writeString("banana\n");
		fu.close();
		eq(sys.io.File.getContent(filename), "cherry\nbanana\n");

		// File.update should create the file if it doesn't exist
		sys.FileSystem.deleteFile(filename);
		var fu = sys.io.File.update(filename);
		fu.close();
		t(sys.FileSystem.exists(filename));

		sys.FileSystem.deleteFile(filename);
		#else
		eq(1, 1);
		#end

	}
}
