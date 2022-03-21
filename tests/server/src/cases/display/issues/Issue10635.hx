package cases.display.issues;

class Issue10635 extends DisplayTestCase {
	/**
		class C {
			public function new() {}
		}

		function main() {
			Something.append([new C()], [new C()]);
		}
	**/
	function test(_) {
		var args = ["-main", "Main", "--display", "Main.hx@0@diagnostics"];
		vfs.putContent("Something.hx", getTemplate("issues/Issue10635/Something.hx"));
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		Assert.isTrue(lastResult.stderr.length == 2); // dumb, but we don't have a proper diagnostics structure in these tests
	}

	/**
		class C {
			public function new() {}
		}

		function main() {
			Something.append([new C()], [new C()]);
		}
	**/
	function testGenericClassPerMethod(_) {
		var args = ["-main", "Main", "--display", "Main.hx@0@diagnostics"];
		vfs.putContent("Something.hx", "@:genericClassPerMethod " + getTemplate("issues/Issue10635/Something.hx"));
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		Assert.isTrue(lastResult.stderr.length == 2); // dumb, but we don't have a proper diagnostics structure in these tests
	}
}