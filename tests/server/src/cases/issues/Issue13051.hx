package cases.issues;

import haxe.display.Protocol;

class Issue13051 extends TestCase {
	function getBaseArgs(define:String) {
		return [
			"--js", "no.js", "--no-output",
			"-D", "disable-hxb-optimizations",
			"-D", define
		];
	}

	function getArgs(define:String) {
		return getBaseArgs(define).concat([
			"-main", "WithDependency",
			"--macro", "haxe.macro.Context.registerModuleDependency(\"Dependency\", \"res/dep.dep\")"
		]);
	}

	function putFiles() {
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		vfs.putContent("WithDependency.hx", getTemplate("WithDependency.hx"));
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		vfs.putContent("res/dep.dep", "");
	}

	function testStaleContext(_) {
		putFiles();
		runHaxeJson([], Methods.Initialize, {staleContextMaxAge: 6});

		var argsA = getArgs("a");
		var argsB = getArgs("b");

		runHaxe(argsA);
		assertSuccess();
		runHaxe(argsB);
		assertSuccess();

		Sys.sleep(3);

		runHaxeJson(getBaseArgs("b"), DisplayMethods.Hover, {file: new FsPath("Empty.hx"), offset: 0});
		assertSuccess();

		Sys.sleep(4);

		runHaxeJson([], ServerMethods.Contexts, null);

		Sys.sleep(0.1);

		// Used to crash: Could not find dependency $DEP.<path>res/dep.dep of Dependency in the cache
		runHaxe(argsB);
		assertSuccess();
		assertReuse("Dependency");

		// Used to crash as a follow-up of the above: modules were left in an unknown cache state
		runHaxe(argsB);
		assertSuccess();
	}

	function testDependencyChange(_) {
		putFiles();
		var args = getArgs("a");

		runHaxe(args);
		assertSuccess();
		runHaxe(args);
		assertReuse("Dependency");

		Sys.sleep(1);
		vfs.putContent("res/dep.dep", "changed");

		runHaxe(args);
		assertSuccess();
		Assert.isFalse(hasMessage("reusing Dependency"));

		// Must be reused again once the fake module is replaced
		runHaxe(args);
		assertReuse("Dependency");
	}
}
