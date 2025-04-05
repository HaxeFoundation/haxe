package cases.display.issues;

class Issue8666 extends DisplayTestCase {
	function test(_) {
		vfs.putContent("cp1/HelloWorld.hx", getTemplate("HelloWorld.hx"));
		vfs.putContent("cp2/MyClass.hx", "class MyClass { }");
		var args = ["-cp", "cp1", "--interp"];
		runHaxeJson(args, DisplayMethods.Completion, {file: new FsPath("cp1/HelloWorld.hx"), offset: 75, wasAutoTriggered: false});
		var completion = parseCompletion();
		assertHasNoCompletion(completion, module -> switch (module.kind) {
			case Type: module.args.path.typeName == "MyClass";
			case _: false;
		});
		runHaxeJson(args.concat(["-cp", "cp2"]), DisplayMethods.Completion, {file: new FsPath("cp1/HelloWorld.hx"), offset: 75, wasAutoTriggered: false});
		var completion = parseCompletion();
		assertHasCompletion(completion, module -> switch (module.kind) {
			case Type: module.args.path.typeName == "MyClass";
			case _: false;
		});
	}

	function testLib(_) {
		vfs.putContent("cp1/HelloWorld.hx", getTemplate("HelloWorld.hx"));
		vfs.putContent("cp2/MyClass.hx", "class MyClass { }");
		var args = ["-cp", "cp1", "--interp"];
		runHaxeJson(args, DisplayMethods.Completion, {file: new FsPath("cp1/HelloWorld.hx"), offset: 75, wasAutoTriggered: false});
		var completion = parseCompletion();
		assertHasNoCompletion(completion, module -> switch (module.kind) {
			case Type: module.args.path.typeName == "MyClass";
			case _: false;
		});
		runHaxeJson(args.concat(["-cp", "cp2", "-D", "imalibrary"]), DisplayMethods.Completion,
			{file: new FsPath("cp1/HelloWorld.hx"), offset: 75, wasAutoTriggered: false});
		var completion = parseCompletion();
		assertHasCompletion(completion, module -> switch (module.kind) {
			case Type: module.args.path.typeName == "MyClass";
			case _: false;
		});
	}
}