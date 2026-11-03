package cases.display.issues;

import haxe.display.JsonModuleTypes;

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
		var args = ["-main", "Main"];
		vfs.putContent("Something.hx", getTemplate("issues/Issue10635/Something.hx"));
		Assert.equals(0, runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}).length);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		Assert.equals(0, runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}).length);
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
		var args = ["-main", "Main"];
		vfs.putContent("Something.hx", "@:genericClassPerMethod " + getTemplate("issues/Issue10635/Something.hx"));
		Assert.equals(0, runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}).length);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		Assert.equals(0, runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}).length);
	}

	function testGenericAddition(_) {
		var args = ["-main", "Main"];
		vfs.putContent("GenericMethod.hx", getTemplate("GenericMethod.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue10635/MainBefore.hx"));
		runHaxe(args);
		vfs.putContent("Main.hx", getTemplate("issues/Issue10635/MainAfter.hx"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		// Note: We only have to run this once to reproduce because ServerMethods.Type will call cl_restore anyway
		runHaxe(args);
		var contexts:Array<HaxeServerContext> = runHaxeJson(args, ServerMethods.Contexts, null);
		utest.Assert.equals(1, contexts.length);
		var sig = contexts[0].signature;
		var type:JsonModuleType<JsonClass> = runHaxeJson(args, ServerMethods.Type, {signature: sig, modulePath: "GenericMethod", typeName: "GenericMethod"});
		var statics = type.args.statics;
		Assert.isTrue(statics.exists(cf -> cf.name == "f"));
		Assert.isTrue(statics.exists(cf -> cf.name == "f_Class<Main>"));
	}

	function testGenericInstanceAddition(_) {
		var args = ["-main", "Main"];
		vfs.putContent("GenericInstanceMethod.hx", getTemplate("GenericInstanceMethod.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue10635/MainInstanceBefore.hx"));
		runHaxe(args);
		vfs.putContent("Main.hx", getTemplate("issues/Issue10635/MainInstanceAfter.hx"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		// Note: We only have to run this once to reproduce because ServerMethods.Type will call cl_restore anyway
		runHaxe(args);
		final contexts = runHaxeJson(args, ServerMethods.Contexts, null);
		Assert.notNull(contexts);
		utest.Assert.equals(1, contexts.length);
		final sig = contexts[0].signature;
		final type:JsonModuleType<JsonClass> = runHaxeJson(args, ServerMethods.Type,
			{signature: sig, modulePath: "GenericInstanceMethod", typeName: "GenericInstanceMethod"});
		var fields = type.args.fields;
		Assert.isTrue(fields.exists(cf -> cf.name == "f"));
		Assert.isTrue(fields.exists(cf -> cf.name == "f_Class<Main>"));
	}
}
