import utest.Assert;
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
}
