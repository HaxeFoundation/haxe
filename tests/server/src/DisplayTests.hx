import haxe.display.JsonModuleTypes;
import haxe.display.Protocol;
import haxe.PosInfos;
import haxe.display.Server;
import utest.Assert;
import utest.Assert.*;
import haxe.display.Display;
import haxe.display.FsPath;

@:timeout(5000)
// TODO: somebody has to clean this up
class DisplayTests extends HaxeServerTestCase {
	function testIssue7262() {
		var content = 'class Main {
			static public function main() {
				var x:haxe.extern.EitherType<haxe.PosInfos, () -> Void> = {-1-}
			}
		}';
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[1],
			wasAutoTriggered: true
		});
		var result = parseCompletion().result;
		equals("TAnonymous", result.mode.args.expectedTypeFollowed.args.params[0].kind);
	}

	function testIssue7305() {
		var content = 'class Main {
	static public function main() {
		new Map{-1-}
	}
}';
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[1],
			wasAutoTriggered: true
		});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch (item.kind) {
			case Type: item.args.path.pack.length == 0 && item.args.path.typeName == "Map";
			case _: false;
		});
	}

	function testIssue7317() {
		var content = 'class Main {
	public static function main() {
		var obj = {};
		obj.{-1-}
	}
}';
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[1],
			wasAutoTriggered: true
		});
		var result = parseCompletion();
		Assert.equals("obj", result.result.mode.args.item.args.name);
	}

	function testIssue7923() {
		vfs.putContent("TreeItem.hx", getTemplate("issues/Issue7923/TreeItem.hx"));
		var content = getTemplate("issues/Issue7923/Main.hx");
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[1],
			wasAutoTriggered: true
		});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch (item.kind) {
			case EnumAbstractField: item.args.field.name == "Collapsed";
			case _: false;
		});
	}

	function testIssue8061() {
		var content = 'class Main {
	static function main() {
		new sys.io.Process({-1-})
	}
}';
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.SignatureHelp, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[1],
			wasAutoTriggered: true
		});
		var result = parseSignatureHelp();
		Assert.isTrue(result.result.signatures[0].documentation != null);
	}

	function testIssue8194() {
		var content = 'class Main {
	static function main() {
		switch ("p") {
			case "p"{-1-}
				"foo";
		}
	}
}';
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[1],
			wasAutoTriggered: true
		});
		var result = parseCompletion();
		Assert.equals(null, result.result);
	}

	function testIssue8381() {
		var content = 'class Main {
	static function main() {
		var f:Foo;
		f.f{-1-}oo();
		f.bar;
	}
}

typedef Foo = {
	/** Test **/
	function foo():Void;

	/** Test **/
	var bar:Int;
}';

		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Hover, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[1]
		});
		var result = parseHover();
		Assert.equals(DisplayItemKind.ClassField, result.result.item.kind);
	}

	function testIssue8438() {
		var content = 'class Main {
	static function main() {
		" ".char{-1-}
	}
}';
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[1],
			wasAutoTriggered: true
		});
		var result = parseCompletion();
		Assert.equals(6, result.result.replaceRange.start.character);
		Assert.equals(10, result.result.replaceRange.end.character);
	}

	function testIssue8602() {
		var content = "class Main {
	static function main() {
		haxe.ds.{-1-}
	}
}";
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {file: new FsPath("Main.hx"), offset: transform.markers[1], wasAutoTriggered: true});
		var result = parseCompletion();
		Assert.equals(Toplevel, result.result.mode.kind);
	}

	function testIssue8644() {
		vfs.putContent("HelloJvm.hx", getTemplate("HelloJvm.hx"));
		var args = ["-cp", ".", "--interp"];
		runHaxeJson(args, ServerMethods.ReadClassPaths, null);
		runHaxeJson(args, DisplayMethods.Completion, {file: new FsPath("HelloJvm.hx"), offset: 55, wasAutoTriggered: false});
		var completion = parseCompletion();
		assertHasNoCompletion(completion, module -> switch (module.kind) {
			case Type: module.args.path.typeName == "Jvm";
			case _: false;
		});
	}

	function testIssue8651() {
		var content = "class Main { static function main() { {-1-}buffer{-2-} } }";
		vfs.putContent("Main.hx", content);
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {file: new FsPath("Main.hx"), offset: transform.markers[2], wasAutoTriggered: true});
		var result = parseCompletion();
		var r = result.result;
		Assert.equals("buffer", r.filterString);
		Assert.equals(transform.markers[1], r.replaceRange.start.character);
		Assert.equals(transform.markers[2], r.replaceRange.end.character);
	}

	function testIssue8657() {
		var content = "class Main { static function main() { var x:{-1-}stream{-2-} } }";
		vfs.putContent("Main.hx", content);
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {file: new FsPath("Main.hx"), offset: transform.markers[2], wasAutoTriggered: true});
		var result = parseCompletion();
		var r = result.result;
		Assert.equals("stream", r.filterString);
		Assert.equals(transform.markers[1], r.replaceRange.start.character);
		Assert.equals(transform.markers[2], r.replaceRange.end.character);
	}

	function testIssue8659() {
		var content = "class Main extends {-1-}StreamTokenizer{-2-} { }";
		vfs.putContent("Main.hx", content);
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {file: new FsPath("Main.hx"), offset: transform.markers[2], wasAutoTriggered: true});
		var result = parseCompletion();
		var r = result.result;
		Assert.equals("StreamTokenizer", r.filterString);
		Assert.equals(transform.markers[1], r.replaceRange.start.character);
		Assert.equals(transform.markers[2], r.replaceRange.end.character);
	}

	function testIssue8666() {
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

	function testIssue8666_lib() {
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

	function testIssue8732() {
		var content = "class Main { static function main() { var ident = \"foo\"; {-1-}i{-2-}dent.{-3-} } }";
		vfs.putContent("Main.hx", content);
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], Methods.Initialize, {maxCompletionItems: 50});
		runHaxeJson([], DisplayMethods.Completion, {file: new FsPath("Main.hx"), offset: transform.markers[2], wasAutoTriggered: true});
		runHaxeJson([], DisplayMethods.Completion, {file: new FsPath("Main.hx"), offset: transform.markers[3], wasAutoTriggered: true});
		runHaxeJson([], DisplayMethods.Completion, {file: new FsPath("Main.hx"), offset: transform.markers[1], wasAutoTriggered: true});
		var result = parseCompletion();
		assertHasNoCompletion(result, item -> switch (item.kind) {
			case ClassField: item.args.field.name == "charAt";
			case _: false;
		});
	}

	function testIssue8805_gotoAbstractPropertyWithInlineGetter() {
		vfs.putContent("Main.hx", getTemplate("issues/Issue8805/Main.hx"));
		var args = ["-main", "Main"];
		runHaxeJson(args, DisplayMethods.GotoDefinition, {file: new FsPath("Main.hx"), offset: 56});
		var result = parseGotoDefinition();
		if (result.result.length == 0) {
			Assert.fail('display/definition failed');
		} else {
			Assert.same({"start": {"line": 7, "character": 12}, "end": {"line": 7, "character": 15}}, result.result[0].range);
		}
	}

	function testIssue8992() {
		var mainHx = Marker.extractMarkers('class Main {
	static func{-1-}tion main() {
	}
}');
		vfs.putContent("Main.hx", mainHx.source);

		runHaxe(["--no-output", "-main", "Main"]);
		runHaxeJson([], DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: mainHx.markers[1]});

		var result = parseHover().result;
		Assert.isNull(result);
	}

	function testIssue8991() {
		var mainHx = 'class Main {
	static function main() {
		C.inst{-1-}ance;
	}
}';
		var cHx = 'class C {
	public static var instance:Int;
}';
		var mainHx = Marker.extractMarkers(mainHx);
		vfs.putContent("Main.hx", mainHx.source);
		vfs.putContent("C.hx", cHx);

		runHaxe(["--no-output", "-main", "Main"]);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("C.hx")});
		runHaxeJson([], DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: mainHx.markers[1]});

		var result = parseHover().result;
		Assert.equals(DisplayItemKind.ClassField, result.item.kind);
	}

	function testIssue9012() {
		vfs.putContent("Some.hx", "class Some { public static function func():String return 'hello'; }");

		var content = "import Some.func; class Main { static function main() { fu{-1-}nc(); } }";
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);

		runHaxe(["--no-output", "-main", "Main"]); // commenting this makes it work
		runHaxeJson([], DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.markers[1]});
		var result = parseHover().result;

		Assert.equals(DisplayItemKind.ClassField, result.item.kind);
	}

	function testIssue9039() {
		vfs.putContent("I.hx", "interface I { var prop(get,never):Int; }");
		vfs.putContent("Main.hx", "class Main { static function main() { var i:I = null; } }");

		runHaxe(["--no-output", "-main", "Main"]);

		var content = "class Main { static function main() { var i:I = null; i.{-1-} } }";
		var transform = Marker.extractMarkers(content);

		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[1],
			wasAutoTriggered: true
		});

		assertHasNoCompletion(parseCompletion(), function(item) {
			return switch item.kind {
				case ClassField: item.args.field.name == "get_prop";
				case _: false;
			}
		});
	}

	function testIssue9047() {
		var transform = Marker.extractMarkers("interface Main { var field(never,s{-1-}et):Int; }");
		vfs.putContent("Main.hx", transform.source);
		var args = ["Main", "-js", "main.js"];
		function parseGotoDefintion():GotoDefinitionResult {
			return haxe.Json.parse(lastResult.stderr).result;
		}
		runHaxeJson(args, DisplayMethods.FindReferences, {file: new FsPath("Main.hx"), offset: transform.markers[1], contents: transform.source});
		Assert.same([], parseGotoDefintion().result);
		runHaxeJson(args, DisplayMethods.FindReferences, {file: new FsPath("Main.hx"), offset: transform.markers[1], contents: transform.source});
		Assert.same([], parseGotoDefintion().result);
	}

	function testIssue9082() {
		var args = ["-cp", ".", "--interp"];

		vfs.putContent("org/Thing.hx", "package org; class Thing {}");
		vfs.putContent("AThing.hx", "class AThing {}");
		vfs.putContent("ThingB.hx", "class ThingB {}");
		runHaxeJson(args, Methods.Initialize, {maxCompletionItems: 2});
		runHaxeJson(args, ServerMethods.ReadClassPaths, null);

		var transform = Marker.extractMarkers("class C extends Thing{-1-}");
		vfs.putContent("C.hx", transform.source);
		runHaxeJson(args, DisplayMethods.Completion, {
			file: new FsPath("C.hx"),
			offset: transform.markers[1],
			wasAutoTriggered: true
		});
		var result = parseCompletion();
		assertHasCompletion(result, function(item) {
			return switch item {
				case {kind: Type, args: {path: {pack: ["org"], typeName: "Thing"}}}: true;
				case _: false;
			}
		});
	}

	function testIssue9087() {
		var content = getTemplate("issues/Issue9087/A.hx");
		var transform = Marker.extractMarkers(content);
		vfs.putContent("A.hx", transform.source);
		var args = ["A", "-js", "main.js"];
		function parseGotoDefintion():GotoDefinitionResult {
			return haxe.Json.parse(lastResult.stderr).result;
		}
		runHaxeJson(args, DisplayMethods.GotoImplementation, {file: new FsPath("A.hx"), offset: transform.markers[1], contents: transform.source});
		var result = parseGotoDefintion().result;
		// TODO: We should use the markers, but I forgot how to get lines and characters from offsets
		// Also That Assert.same doesn't work
		Assert.equals(9, result[0].range.start.line);
		Assert.equals(19, result[0].range.start.character);
		Assert.equals(9, result[0].range.end.line);
		Assert.equals(23, result[0].range.end.character);
		// Assert.same([
		// 	{
		// 		range: {
		// 			start: {line: 9, character: 1},
		// 			end: {line: 11, character: 2}
		// 		}
		// 	}
		// ], result);
	}

	function testIssue9115() {
		var content = getTemplate("issues/Issue9115/A.hx");
		var transform = Marker.extractMarkers(content);
		vfs.putContent("A.hx", transform.source);
		runHaxe(["--no-output", "A"]);
		runHaxeJson([], DisplayMethods.Hover, {
			file: new FsPath("A.hx"),
			offset: transform.markers[1]
		});
		var result = parseHover();
		Assert.equals("A", result.result.item.type.args.path.typeName /* lol */);
	}

	function testIssue7754() {
		var content = '
			class Main {
				static function main() {
					Foo.foo({-1-});
				}
			}
			extern class Foo {
				@:overload(function(?s:String):Void {})
				static function foo(?i:Int):Void;
			}
		';
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.SignatureHelp, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[1],
			wasAutoTriggered: true
		});
		var result = parseSignatureHelp();
		var sigs = result.result.signatures;
		Assert.equals(2, sigs.length);
		Assert.equals('Null<String>', strType(sigs[0].args[0].t));
		Assert.equals('Null<Int>', strType(sigs[1].args[0].t));
	}

	function testIssue9159() {
		var content = '
			@:structInit
			class CustomConstructor {
				public var nope1:String;
				public function new(x:Int = 0) {}
				public function nope2() {}
			}

			@:structInit
			class AutoConstructor {
				public var y:Float;
				public function nope() {}
			}

			class Main {
				static function main() {
					var a:CustomConstructor = {-1-}{};
					var b:AutoConstructor = {-2-}{};
				}
			}
		';
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);

		runHaxeJson([], DisplayMethods.Completion, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[1],
			wasAutoTriggered: true
		});
		var result = parseCompletion().result;
		Assert.equals(1, result.items.length);
		Assert.equals('x', result.items[0].args.field.name);

		runHaxeJson([], DisplayMethods.Completion, {
			file: new FsPath("Main.hx"),
			offset: transform.markers[2],
			wasAutoTriggered: true
		});
		var result = parseCompletion().result;
		Assert.equals(1, result.items.length);
		Assert.equals('y', result.items[0].args.field.name);
	}
}
