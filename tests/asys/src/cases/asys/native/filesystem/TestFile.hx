package cases.asys.native.filesystem;

import asys.native.filesystem.FileSystem;
import asys.native.filesystem.File;

@:depends(cases.asys.native.filesystem.TestFileSystem)
class TestFile extends FsTest {
	function testOpenFile() {
		pass();
	}
}