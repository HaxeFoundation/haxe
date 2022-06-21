package cases;

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
		Assert.isTrue(hasMessage('retyped WithDependency'));
	}

	function testSignature() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("retyper/WithSignatureDependency.hx"));
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
		Assert.isTrue(hasMessage('failed retyping WithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module WithSignatureDependency] [Class WithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}

	function testSignatureInferredArg() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("retyper/WithSignatureDependencyInferredArg.hx"));
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
		Assert.isTrue(hasMessage('failed retyping WithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module WithSignatureDependency] [Class WithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}

	function testSignatureInferredRet() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("retyper/WithSignatureDependencyInferredRet.hx"));
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
		Assert.isTrue(hasMessage('failed retyping WithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module WithSignatureDependency] [Class WithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}

	function testSignatureVariable() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("retyper/WithSignatureDependencyVariable.hx"));
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
		Assert.isTrue(hasMessage('failed retyping WithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module WithSignatureDependency] [Class WithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}

	function testSignatureInferredVariable() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("retyper/WithSignatureDependencyInferredVariable.hx"));
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
		Assert.isTrue(hasMessage('failed retyping WithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module WithSignatureDependency] [Class WithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}

	function testSignatureProperty() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("retyper/WithSignatureDependencyProperty.hx"));
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
		Assert.isTrue(hasMessage('failed retyping WithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module WithSignatureDependency] [Class WithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}

	function testSignatureInferredProperty() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("retyper/WithSignatureDependencyInferredProperty.hx"));
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
		Assert.isTrue(hasMessage('failed retyping WithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module WithSignatureDependency] [Class WithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}

	function testMutual() {
		vfs.putContent("WithMutualDependency.hx", getTemplate("retyper/WithMutualDependency.hx"));
		vfs.putContent("MutualDependency.hx", getTemplate("retyper/MutualDependency.hx"));
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
		Assert.isTrue(hasMessage('retyped WithMutualDependency'));
	}

	function testIndependentEnum() {
		vfs.putContent("IndependentEnum.hx", getTemplate("retyper/IndependentEnum.hx"));
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
		Assert.isTrue(hasMessage('retyped IndependentEnum'));
	}

	function testDependentEnum() {
		vfs.putContent("DependentEnum.hx", getTemplate("retyper/DependentEnum.hx"));
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
		Assert.isTrue(hasMessage('failed retyping DependentEnum'));
		Assert.isTrue(hasMessage('[Module DependentEnum] [Enum DependentEnum] [Field Constructor]: Could not load [Module Dependency]'));
	}

	function testIndependentTypedef() {
		vfs.putContent("IndependentTypedef.hx", getTemplate("retyper/IndependentTypedef.hx"));
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
		Assert.isTrue(hasMessage('retyped IndependentTypedef'));
	}

	function testDependentTypedef() {
		vfs.putContent("DependentTypedef.hx", getTemplate("retyper/DependentTypedef.hx"));
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
		Assert.isTrue(hasMessage('failed retyping DependentTypedef'));
		Assert.isTrue(hasMessage('[Module DependentTypedef] [Typedef DependentTypedef]: Could not load [Module Dependency]'));
	}

	function testAbstractNonSignature() {
		vfs.putContent("AbstractWithDependency.hx", getTemplate("retyper/AbstractWithDependency.hx"));
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
		Assert.isTrue(hasMessage('retyped AbstractWithDependency'));
	}

	function testAbstractSignature() {
		vfs.putContent("AbstractWithSignatureDependency.hx", getTemplate("retyper/AbstractWithSignatureDependency.hx"));
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
		Assert.isTrue(hasMessage('failed retyping AbstractWithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module AbstractWithSignatureDependency] [Abstract AbstractWithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}
}
