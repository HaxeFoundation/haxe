import AsyncMacro.async;

class NoModification extends HaxeServerTestCase {
	public function test() {
		async({
			vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
			var args = ["-main", "HelloWorld.hx", "--no-output", "-js", "no.js"];
			haxe(args);
			haxe(args);
			assertReuse("HelloWorld");
			haxe(args);
			assertReuse("HelloWorld");
		});
	}
}

class Modification extends HaxeServerTestCase {
	public function test() {
		async({
			vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
			var args = ["-main", "HelloWorld.hx", "--no-output", "-js", "no.js"];
			haxe(args);
			vfs.touchFile("HelloWorld.hx");
			haxe(args);
			assertSkipping("HelloWorld");
			assertNotCacheModified("HelloWorld");
		});
	}
}

class Dependency extends HaxeServerTestCase {
	public function test() {
		async({
			vfs.putContent("WithDependency.hx", getTemplate("WithDependency.hx"));
			vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
			var args = ["-main", "WithDependency.hx", "--no-output", "-js", "no.js"];
			haxe(args);
			vfs.touchFile("Dependency.hx");
			haxe(args);
			assertSkipping("WithDependency", "Dependency");
			assertNotCacheModified("Dependency");
			haxe(args);
			assertReuse("Dependency");
			assertReuse("WithDependency");
		});
	}
}

class Main {
	static public function main() {
		Vfs.removeDir("test/cases");
		var runner = new utest.Runner();
		runner.addCase(new NoModification());
		runner.addCase(new Modification());
		runner.addCase(new Dependency());
		utest.ui.Report.create(runner);
		runner.run();
	}
}