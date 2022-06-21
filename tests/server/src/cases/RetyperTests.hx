package cases;

import haxe.display.FsPath;
import haxe.display.Server;
import utest.Assert;

using StringTools;
using Lambda;

class RetyperTests extends TestCase {
	static function getBaseArgs(moduleName:String) {
		return [
			moduleName + ".hx",
			"--no-output",
			"-js",
			"no.js",
			"--macro",
			"haxe.macro.CompilationServer.setModuleCheckPolicy(['" + moduleName + "'], [Retype], false)"
		];
	}

	function testNonSignature() {
		vfs.putContent("WithDependency.hx", getTemplate("WithDependency.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("WithDependency");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('retyped WithDependency'));
	}

	function testSignature() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("retyper/WithSignatureDependency.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("WithSignatureDependency");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('failed retyping WithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module WithSignatureDependency] [Class WithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}

	function testSignatureInferredArg() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("retyper/WithSignatureDependencyInferredArg.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("WithSignatureDependency");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('failed retyping WithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module WithSignatureDependency] [Class WithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}

	function testSignatureInferredRet() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("retyper/WithSignatureDependencyInferredRet.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("WithSignatureDependency");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('failed retyping WithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module WithSignatureDependency] [Class WithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}

	function testSignatureVariable() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("retyper/WithSignatureDependencyVariable.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("WithSignatureDependency");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('failed retyping WithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module WithSignatureDependency] [Class WithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}

	function testSignatureInferredVariable() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("retyper/WithSignatureDependencyInferredVariable.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("WithSignatureDependency");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('failed retyping WithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module WithSignatureDependency] [Class WithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}

	function testSignatureProperty() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("retyper/WithSignatureDependencyProperty.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("WithSignatureDependency");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('failed retyping WithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module WithSignatureDependency] [Class WithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}

	function testSignatureInferredProperty() {
		vfs.putContent("WithSignatureDependency.hx", getTemplate("retyper/WithSignatureDependencyInferredProperty.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("WithSignatureDependency");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('failed retyping WithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module WithSignatureDependency] [Class WithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}

	function testMutual() {
		vfs.putContent("WithMutualDependency.hx", getTemplate("retyper/WithMutualDependency.hx"));
		vfs.putContent("MutualDependency.hx", getTemplate("retyper/MutualDependency.hx"));
		var args = getBaseArgs("WithMutualDependency");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("MutualDependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('retyped WithMutualDependency'));
	}

	function testParent() {
		vfs.putContent("WithParentDependency.hx", getTemplate("retyper/WithParentDependency.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("WithParentDependency");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('failed retyping WithParentDependency'));
		Assert.isTrue(hasMessage('[Module WithParentDependency] [Class WithParentDependency] [Relations]: Could not load [Module Dependency]'));
	}

	function testInterface() {
		vfs.putContent("WithInterfaceDependency.hx", getTemplate("retyper/WithInterfaceDependency.hx"));
		vfs.putContent("InterfaceDependency.hx", getTemplate("retyper/InterfaceDependency.hx"));
		var args = getBaseArgs("WithInterfaceDependency");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("InterfaceDependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('failed retyping WithInterfaceDependency'));
		Assert.isTrue(hasMessage('[Module WithInterfaceDependency] [Class WithInterfaceDependency] [Relations]: Could not load [Module InterfaceDependency]'));
	}

	function testIndependentEnum() {
		vfs.putContent("IndependentEnum.hx", getTemplate("retyper/IndependentEnum.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("IndependentEnum");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('retyped IndependentEnum'));
	}

	function testDependentEnum() {
		vfs.putContent("DependentEnum.hx", getTemplate("retyper/DependentEnum.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("DependentEnum");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('failed retyping DependentEnum'));
		Assert.isTrue(hasMessage('[Module DependentEnum] [Enum DependentEnum] [Field Constructor]: Could not load [Module Dependency]'));
	}

	function testIndependentTypedef() {
		vfs.putContent("IndependentTypedef.hx", getTemplate("retyper/IndependentTypedef.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("IndependentTypedef");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('retyped IndependentTypedef'));
	}

	function testDependentTypedef() {
		vfs.putContent("DependentTypedef.hx", getTemplate("retyper/DependentTypedef.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("DependentTypedef");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('failed retyping DependentTypedef'));
		Assert.isTrue(hasMessage('[Module DependentTypedef] [Typedef DependentTypedef]: Could not load [Module Dependency]'));
	}

	function testAbstractNonSignature() {
		vfs.putContent("AbstractWithDependency.hx", getTemplate("retyper/AbstractWithDependency.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("AbstractWithDependency");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('retyped AbstractWithDependency'));
	}

	function testAbstractSignature() {
		vfs.putContent("AbstractWithSignatureDependency.hx", getTemplate("retyper/AbstractWithSignatureDependency.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = getBaseArgs("AbstractWithSignatureDependency");
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		Assert.isTrue(hasMessage('failed retyping AbstractWithSignatureDependency'));
		Assert.isTrue(hasMessage('[Module AbstractWithSignatureDependency] [Abstract AbstractWithSignatureDependency] [Field test]: Could not load [Module Dependency]'));
	}
}
