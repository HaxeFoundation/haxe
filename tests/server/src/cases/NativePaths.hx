package cases;

class NativePaths extends TestCase {
	/* the native names are applied once to a declaration that the server keeps */
	function testEnumConstructor() {
		vfs.putContent("Main.hx", getTemplate("NativePaths/Main.hx"));
		vfs.putContent("Mac.hx", getTemplate("NativePaths/Mac.hx"));
		var args = ["-main", "Main", "--js", "main.js", "--no-output", "--macro", "Mac.init()"];
		runHaxe(args);
		assertHasPrint("realPath MyEnum 0");
		runHaxe(args);
		assertHasPrint("realPath MyEnum 1");
		runHaxe(args);
		assertHasPrint("realPath MyEnum 1");
	}
}
