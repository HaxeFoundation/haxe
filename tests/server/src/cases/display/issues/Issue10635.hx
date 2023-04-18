package cases.display.issues;

import haxe.display.JsonModuleTypes;
import haxe.Json;

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

	function testGenericAddition(_) {
		var args = ["-main", "Main"];
		vfs.putContent("GenericMethod.hx", getTemplate("GenericMethod.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue10635/MainBefore.hx"));
		runHaxe(args);
		vfs.putContent("Main.hx", getTemplate("issues/Issue10635/MainAfter.hx"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		// Note: We only have to run this once to reproduce because ServerMethods.Type will call cl_restore anyway
		runHaxe(args);
		runHaxeJson(args, ServerMethods.Contexts, null);
		var contexts:Array<HaxeServerContext> = Json.parse(lastResult.stderr).result.result;
		utest.Assert.equals(1, contexts.length);
		var sig = contexts[0].signature;
		runHaxeJson(args, ServerMethods.Type, {signature: sig, modulePath: "GenericMethod", typeName: "GenericMethod"});
		var type:JsonModuleType<JsonClass> = Json.parse(lastResult.stderr).result.result;
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
		var contexts = null;
		runHaxeJsonCb(args, ServerMethods.Contexts, null, r -> contexts = r);
		Assert.notNull(contexts);
		utest.Assert.equals(1, contexts.length);
		var sig = contexts[0].signature;
		var type:JsonModuleType<JsonClass> = null;
		runHaxeJsonCb(args, ServerMethods.Type, {signature: sig, modulePath: "GenericInstanceMethod", typeName: "GenericInstanceMethod"}, r -> type = r);
		var fields = type.args.fields;
		Assert.isTrue(fields.exists(cf -> cf.name == "f"));
		Assert.isTrue(fields.exists(cf -> cf.name == "f_Class<Main>"));
	}
}
