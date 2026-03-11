package cases.display.issues;

import haxe.display.Server.ServerMethods;

class Issue8644 extends DisplayTestCase {
	function test(_) {
		vfs.putContent("HelloJvm.hx", getTemplate("HelloJvm.hx"));
		var args = ["-cp", ".", "--interp"];
		runHaxeJson(args, ServerMethods.ReadClassPaths, {wait: true});
		var completion = runHaxeJson(args, DisplayMethods.Completion, {file: new FsPath("HelloJvm.hx"), offset: 55, wasAutoTriggered: false});
		assertHasNoCompletion(completion, module -> switch (module.kind) {
			case Type: module.args.path.typeName == "Jvm";
			case _: false;
		});
	}
}