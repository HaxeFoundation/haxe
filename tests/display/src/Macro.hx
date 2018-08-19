import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.io.Path;

using sys.FileSystem;
using Lambda;
using StringTools;

class Macro {
	static function buildTestCase():Array<Field> {
		var fields = Context.getBuildFields();
		var markerRe = ~/{-(\d+)-}/g;
		var testCases = [];
		var c = Context.getLocalClass().get();
		for (field in fields) {
			var markers = [];
			var posAcc = 0;
			if (field.doc == null) {
				continue;
			}
			var doc = (c.pack.length > 0 ? "package " + c.pack.join(".") + ";\n" : "");
			if (field.meta.exists(function(meta) return meta.name == ":funcCode")) {
				doc += "class Main { static function main() { " + field.doc + "}}";
			} else {
				doc += field.doc;
			}
			var src = markerRe.map(doc, function(r) {
				var p = r.matchedPos();
				var name = r.matched(1);
				var pos = p.pos - posAcc;
				posAcc += p.len;
				markers.push(macro $v{Std.parseInt(name)} => $v{pos});
				return "";
			});
			var markers = markers.length > 0 ? macro $a{markers} : macro new Map();
			testCases.push(macro function() {
				ctx = new DisplayTestContext($v{Context.getPosInfos(c.pos).file}, $v{field.name}, $v{src}, $markers);
				$i{field.name}();
			});
		}

		fields.push((macro class {
			public function new() {
				testName = $v{c.name};
				numTests = 0;
				numFailures = 0;
				this.methods = $a{testCases};
			}
		}).fields[0]);

		return fields;
	}

	macro static public function getCases(pack:String) {
		var cases = [];
		var singleCase = haxe.macro.Compiler.getDefine("test");
		function loop(pack:Array<String>) {
			var path = Context.resolvePath(Path.join(pack));
			for (file in sys.FileSystem.readDirectory(path)) {
				if (singleCase != null && !file.endsWith(singleCase + ".hx")) {
					continue;
				}
				var p = new haxe.io.Path(file);
				if (p.ext == "hx") {
					var tp = {pack: pack, name: p.file};
					cases.push(macro { name:$v{tp.name}, exec:new $tp() });
				} else if(Path.join([path, file]).isDirectory()) {
					loop(pack.concat([file]));
				}
			}
		}
		loop(pack.split('.'));
		return macro $a{cases};
	}
}
