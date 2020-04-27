package cases.issues;

class Issue8748 extends TestCase {
	function test(_) {
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		vfs.putContent("WithDependency.hx", getTemplate("WithDependency.hx"));
		vfs.putContent("res/dep.dep", "");
		var args = [
			"-main",
			"WithDependency",
			"--interp",
			"--macro",
			"haxe.macro.Context.registerModuleDependency(\"Dependency\", \"res/dep.dep\")"
		];
		runHaxeJson(args, ServerMethods.Configure, {noModuleChecks: true});
		runHaxe(args);
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("WithDependency.hx"), offset: 65});
		assertReuse("Dependency");
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("res/dep.dep")});
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("WithDependency.hx"), offset: 65});
		// check messages manually because module file contains awkward absolute path
		var r = ~/skipping Dependency\(.*dep.dep\)/;
		Assert.isTrue(messages.exists(message -> r.match(message)));
	}
}