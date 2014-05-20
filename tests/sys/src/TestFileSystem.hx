import sys.FileSystem;

class TestFileSystem extends haxe.unit.TestCase {
	function testSimpleDir():Void {
		assertFalse(FileSystem.exists("testCreateDirectory"));
		FileSystem.createDirectory("testCreateDirectory");
		assertTrue(FileSystem.exists("testCreateDirectory"));
		assertTrue(FileSystem.isDirectory("testCreateDirectory"));
		FileSystem.deleteDirectory("testCreateDirectory");
		assertFalse(FileSystem.exists("testCreateDirectory"));
	}

	function testParentDir():Void {
		assertFalse(FileSystem.exists("../testCreateDirectory"));
		FileSystem.createDirectory("../testCreateDirectory");
		assertTrue(FileSystem.exists("../testCreateDirectory"));
		assertTrue(FileSystem.isDirectory("../testCreateDirectory"));
		FileSystem.deleteDirectory("../testCreateDirectory");
		assertFalse(FileSystem.exists("../testCreateDirectory"));
	}
}