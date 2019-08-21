import haxe.PosInfos;
import haxe.display.Server;
import utest.Assert;
import utest.Assert.*;
import haxe.display.Display;
import haxe.display.FsPath;

@:timeout(5000)
// TODO: somebody has to clean this up
class DisplayTests extends HaxeServerTestCase {
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

	function complete<S, T>(content:String, markerIndex:Int, cb:(response:CompletionResponse<S, T>, markers:Map<Int, Int>) -> Void) {
		var transform = Marker.extractMarkers(content);
		vfs.putContent("Main.hx", transform.source);
		runHaxeJson([], DisplayMethods.Completion, {file: new FsPath("Main.hx"), offset: transform.markers[markerIndex], wasAutoTriggered: true}, function() {
			var result = parseCompletion();
			cb(result.result, transform.markers);
		});
	}

	function checkReplaceRange<S, T>(markers:Map<Int, Int>, startIndex:Int, endIndex:Int, response:CompletionResponse<S, T>, ?p:PosInfos) {
		equals(markers[startIndex], response.replaceRange.start.character, p);
		equals(markers[endIndex], response.replaceRange.end.character, p);
	}

	function testIssue8669_type() {
		complete("{-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("{-1-}cl{-2-}", 2);
		equals("cl", response.filterString);
		checkReplaceRange(markers, 1, 2, response);
	}

	function testIssue8669_modifier() {
		complete("extern {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("extern {-1-}cl{-2-}", 2);
		equals("cl", response.filterString);
		checkReplaceRange(markers, 1, 2, response);
	}

	function testIssue8669_extends() {
		complete("class C extends {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("class C extends {-1-}Cl{-2-}", 2);
		equals("Cl", response.filterString);
		checkReplaceRange(markers, 1, 2, response);

		complete("class C {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("class C {-1-}ex{-2-}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("ex", response.filterString);

		complete("class C {-1-}ext{-2-} {}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("ext", response.filterString);
	}

	function testIssue8669_implements() {
		complete("class C implements {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("class C implements {-1-}Cl{-2-}", 2);
		equals("Cl", response.filterString);
		checkReplaceRange(markers, 1, 2, response);

		complete("class C {-1-} {}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("class C {-1-}impl{-2-} {}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("impl", response.filterString);
	}

	function testIssue8669_import() {
		complete("import {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("import {-1-}Cl{-2-}", 2);
		// equals("Cl", response.filterString);
		checkReplaceRange(markers, 1, 2, response);
	}

	function testIssue8669_using() {
		complete("using {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("using {-1-}Cl{-2-}", 2);
		// equals("Cl", response.filterString);
		checkReplaceRange(markers, 1, 2, response);
	}

	function testIssue8669_to() {
		complete("abstract A(String) to {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("abstract A(String) to {-1-} { }", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("abstract A(String) to {-1-}Cl{-2-}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);

		complete("abstract A(String) to {-1-}Cl{-2-} { }", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);
	}

	function testIssue8669_from() {
		complete("abstract A(String) from {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("abstract A(String) from {-1-} { }", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("abstract A(String) from {-1-}Cl{-2-}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);

		complete("abstract A(String) from {-1-}Cl{-2-} { }", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);
	}

	function testIssue8669_structuralExtension() {
		complete("typedef Main = { } & {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("typedef Main = { } & {-1-}Cl{-2-}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);

		complete("typedef Main = { > {-1-}", 1);
		checkReplaceRange(markers, 1, 1, response);

		complete("typedef Main = { > {-1-}Cl{-2-}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);
	}

	function testIssue8669_fields() {
		complete('class Main { static function main() "".{-1-}', 1);
		checkReplaceRange(markers, 1, 1, response);

		complete('class Main { static function main() "".{-1-}char', 1);
		checkReplaceRange(markers, 1, 1, response);

		complete('class Main { static function main() "".{-1-}char{-2-}', 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("char", response.filterString);
	}

	function testIssue8669_override() {
		complete("import haxe.io.Bytes; class Main extends Bytes { static function main() { } override {-1-}}", 1);
		checkReplaceRange(markers, 1, 1, response);
		equals("", response.filterString);

		complete("import haxe.io.Bytes; class Main extends Bytes { static function main() { } override {-1-}get{-2-}}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("get", response.filterString);
	}

	function testIssue8669_typedef() {
		complete("typedef Foo = {-1-}
		", 1);
		checkReplaceRange(markers, 1, 1, response);
		equals("", response.filterString);

		complete("typedef Foo = {-1-}Cl{-2-}
		", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);
	}

	function testIssue8669_typehint() {
		complete("class Main { static function main() { var t:{-1-} }}", 1);
		checkReplaceRange(markers, 1, 1, response);
		equals("", response.filterString);

		complete("class Main { static function main() { var t:{-1-}Cl{-2-} }}", 2);
		checkReplaceRange(markers, 1, 2, response);
		equals("Cl", response.filterString);
	}
}
