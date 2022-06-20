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

	function testSignatureInferredArg() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("WithSignatureDependencyInferredArg.hx"));
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

	function testSignatureInferredRet() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("WithSignatureDependencyInferredRet.hx"));
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

	function testSignatureVariable() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("WithSignatureDependencyVariable.hx"));
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

	function testSignatureInferredVariable() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("WithSignatureDependencyInferredVariable.hx"));
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

	function testSignatureProperty() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("WithSignatureDependencyProperty.hx"));
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

	function testSignatureInferredProperty() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("WithSignatureDependencyInferredProperty.hx"));
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

	function testIndependentEnum() {
		vfs.putContent("IndependentEnum.hx", getTemplate("IndependentEnum.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = [
			"IndependentEnum.hx",
			"--no-output",
			"-js",
			"no.js",
			"--macro",
			"haxe.macro.CompilationServer.setModuleCheckPolicy(['IndependentEnum'], [Retype], false)"
		];
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('Retyping module IndependentEnum'));
		Assert.isTrue(hasMessage('Retyped module IndependentEnum'));
	}

	function testDependentEnum() {
		vfs.putContent("DependentEnum.hx", getTemplate("DependentEnum.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = [
			"DependentEnum.hx",
			"--no-output",
			"-js",
			"no.js",
			"--macro",
			"haxe.macro.CompilationServer.setModuleCheckPolicy(['DependentEnum'], [Retype], false)"
		];
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('Retyping module DependentEnum'));
		Assert.isTrue(hasMessage('Failed retyping module DependentEnum'));
	}

	function testIndependentTypedef() {
		vfs.putContent("IndependentTypedef.hx", getTemplate("IndependentTypedef.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = [
			"IndependentTypedef.hx",
			"--no-output",
			"-js",
			"no.js",
			"--macro",
			"haxe.macro.CompilationServer.setModuleCheckPolicy(['IndependentTypedef'], [Retype], false)"
		];
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('Retyping module IndependentTypedef'));
		Assert.isTrue(hasMessage('Retyped module IndependentTypedef'));
	}

	function testDependentTypedef() {
		vfs.putContent("DependentTypedef.hx", getTemplate("DependentTypedef.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = [
			"DependentTypedef.hx",
			"--no-output",
			"-js",
			"no.js",
			"--macro",
			"haxe.macro.CompilationServer.setModuleCheckPolicy(['DependentTypedef'], [Retype], false)"
		];
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('Retyping module DependentTypedef'));
		Assert.isTrue(hasMessage('Failed retyping module DependentTypedef'));
	}

	function testAbstractNonSignature() {
		vfs.putContent("AbstractWithDependency.hx", getTemplate("AbstractWithDependency.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = [
			"AbstractWithDependency.hx",
			"--no-output",
			"-js",
			"no.js",
			"--macro",
			"haxe.macro.CompilationServer.setModuleCheckPolicy(['AbstractWithDependency'], [Retype], false)"
		];
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('Retyped module AbstractWithDependency'));
	}

	function testAbstractSignature() {
		vfs.putContent("AbstractWithSignatureDependency.hx", getTemplate("AbstractWithSignatureDependency.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = [
			"AbstractWithSignatureDependency.hx",
			"--no-output",
			"-js",
			"no.js",
			"--macro",
			"haxe.macro.CompilationServer.setModuleCheckPolicy(['AbstractWithSignatureDependency'], [Retype], false)"
		];
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('Retyping module AbstractWithSignatureDependency'));
		Assert.isTrue(hasMessage('Failed retyping module AbstractWithSignatureDependency'));
	}
}
