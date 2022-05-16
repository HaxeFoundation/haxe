package cases;

import haxe.display.Display;
import haxe.display.FsPath;
import haxe.display.Server;
import utest.Assert;

using StringTools;
using Lambda;

class RetyperTests extends TestCase {
	function testNonSignature() {
		vfs.putContent("WithDependency.hx", getTemplate("WithDependency.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = [
			"-main",
			"WithDependency.hx",
			"--no-output",
			"-js",
			"no.js",
			"--macro",
			"haxe.macro.CompilationServer.setModuleCheckPolicy(['WithDependency'], [Retype], false)"
		];
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('Retyped module WithDependency'));
	}

	function testSignature() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("WithSignatureDependency.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = [
			"-main",
			"WithSignatureDependency.hx",
			"--no-output",
			"-js",
			"no.js",
			"--macro",
			"haxe.macro.CompilationServer.setModuleCheckPolicy(['WithSignatureDependency'], [Retype], false)"
		];
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('Retyping module WithSignatureDependency'));
		Assert.isTrue(hasMessage('Failed retyping module WithSignatureDependency'));
	}

	function testMutual() {
		vfs.putContent("WithMutualDependency.hx", getTemplate("WithMutualDependency.hx"));
		vfs.putContent("MutualDependency.hx", getTemplate("MutualDependency.hx"));
		var args = [
			"-main",
			"WithMutualDependency.hx",
			"--no-output",
			"-js",
			"no.js",
			"--macro",
			"haxe.macro.CompilationServer.setModuleCheckPolicy(['WithMutualDependency'], [Retype], false)"
		];
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("MutualDependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('Retyping module WithMutualDependency'));
		Assert.isTrue(hasMessage('Retyped module WithMutualDependency'));
	}
}
