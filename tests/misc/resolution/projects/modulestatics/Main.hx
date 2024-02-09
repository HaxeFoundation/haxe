import utest.Assert;

import RootMod1;
import RootMod3.lowerCase3;
import RootMod3.UpperCase3;
import RootMod4.lowerCase as lowerCase4;
import RootMod4.UpperCase as UpperCase4;
import pack.Mod1;
import ModWithStaticAndClassStatic2;

class Main extends utest.Test {
	function testImportedModule() {
		Assert.equals("RootMod1.lowerCase", lowerCase());
		Assert.equals("RootMod1.UpperCase", UpperCase());
		Assert.equals("pack.Mod1.lowerCasePack", lowerCasePack());
		Assert.equals("pack.Mod1.UpperCasePack", UpperCasePack());
	}

	function testUnimportedRootModule() {
		Assert.equals("RootMod2.lowerCase", RootMod2.lowerCase());
		Assert.equals("RootMod2.UpperCase", RootMod2.UpperCase());
	}

	function testUnimportedRootModuleWithStd() {
		Assert.equals("RootMod2.lowerCase", std.RootMod2.lowerCase());
		Assert.equals("RootMod2.UpperCase", std.RootMod2.UpperCase());
	}

	function testUnimportedPackModule() {
		Assert.equals("pack.Mod2.lowerCasePack", pack.Mod2.lowerCasePack());
		Assert.equals("pack.Mod2.UpperCasePack", pack.Mod2.UpperCasePack());
	}

	function testUnimportedPackModuleWithStd() {
		Assert.equals("pack.Mod2.lowerCasePack", std.pack.Mod2.lowerCasePack());
		Assert.equals("pack.Mod2.UpperCasePack", std.pack.Mod2.UpperCasePack());
	}

	function testImportedFunction() {
		Assert.equals("RootMod3.lowerCase", lowerCase3());
		Assert.equals("RootMod3.UpperCase", UpperCase3());
	}

	function testImportedFunctionAliased() {
		Assert.equals("RootMod4.lowerCase", lowerCase4());
		Assert.equals("RootMod4.UpperCase", UpperCase4());
	}

	function testPrivate() {
		Assert.equals("ModWithPrivate.f", ModWithPrivate.f());
	}

	function testUnimportedModuleStaticBeforeMainClassStatic() {
		Assert.equals("ModWithStaticAndClassStatic.lowerCaseMod", ModWithStaticAndClassStatic.lowerCaseMod());
		Assert.equals("ModWithStaticAndClassStatic.UpperCaseMod", ModWithStaticAndClassStatic.UpperCaseMod());
	}

	function testImportedClassStaticBeforeModuleStatic() {
		Assert.equals("ModWithStaticAndClassStatic2.ModWithStaticAndClassStatic2.lowerCaseMod", ModWithStaticAndClassStatic2.lowerCaseMod());
		Assert.equals("ModWithStaticAndClassStatic2.ModWithStaticAndClassStatic2.UpperCaseMod", ModWithStaticAndClassStatic2.UpperCaseMod());
	}

	function testModuleWithStaticsResolvesToMainType() {
		Assert.equals("ModWithStaticAndClassStatic", Type.getClassName(ModWithStaticAndClassStatic));
		Assert.equals("ModWithStaticAndClassStatic2", Type.getClassName(ModWithStaticAndClassStatic2));
		Assert.equals("pack.Mod2", Type.getClassName(pack.Mod2));
	}

	function testMacro() {
		Assert.same([
			"lowerCase",
			"UpperCase",
			"Macro.lowerCase",
			"Macro.UpperCase",
		], Macro.getCalls());

		// force build
		(null : C1);
		(null : C2);
		(null : C3);
		(null : C4);
		var builds = Macro.getBuilds();
		builds.sort(Reflect.compare);
		trace(builds);
		Assert.same([
			"Build",
			"Macro.Build",
			"Macro.build",
			"build",
		], builds);
	}

	static function main() {
		utest.UTest.run([
			new Main(),
			new Wildcard(),
			new pack.inner.Test(),
		]);
	}
}

@:build(Macro.build()) private class C1 {}
@:build(Macro.Build()) private class C2 {}
@:build(Macro.Macro.build()) private class C3 {}
@:build(Macro.Macro.Build()) private class C4 {}