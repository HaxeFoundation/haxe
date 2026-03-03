package cases.display.issues;

class DisplayPackage extends TestCase {
	function testRootPackage(_) {
		vfs.putContent("src/C1.hx", "");
		final res = runHaxeJson(["-p", "src"], DisplayMethods.DeterminePackage, {file: new haxe.display.FsPath("src/C1.hx")});
		Assert.equals(0, res.length);
	}

	function testSubPackage(_) {
		vfs.putContent("src/pack/C2.hx", "");
		final res = runHaxeJson(["-p", "src"], DisplayMethods.DeterminePackage, {file: new haxe.display.FsPath("src/pack/C2.hx")});
		Assert.equals(1, res.length);
		Assert.equals("pack", res[0]);
	}

	function testSubPackageNoClasspath(_) {
		vfs.putContent("src/pack/C2.hx", "");
		final res = runHaxeJson([], DisplayMethods.DeterminePackage, {file: new haxe.display.FsPath("src/pack/C2.hx")});
		Assert.isTrue(res.contains("pack"));
	}
}
