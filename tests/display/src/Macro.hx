import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.io.Path;

using sys.FileSystem;
using Lambda;
using StringTools;

class Macro {
	#if macro
	static function buildTestCase():Array<Field> {
		var fields = Context.getBuildFields();
		var c = Context.getLocalClass().get();
		for (field in fields) {
			if (field.doc == null) {
				continue;
			}

			var doc = if (field.meta.exists(function(meta) return meta.name == ":funcCode")) {
				"class Main { static function main() { " + field.doc + "}}";
			} else {
				field.doc;
			};

			doc = StringTools.replace(doc, "**\\/", "**/");
			var transform = Marker.extractMarkers(doc);
			var markers = transform.markers.length > 0 ? macro $a{transform.markers} : macro new Map();
			var filename = Context.getPosInfos(c.pos).file;
			for (meta in field.meta) {
				if (meta.name == ":filename") {
					switch (meta.params[0].expr) {
						case EConst(CString(s)):
							filename = Path.directory(filename) + "/" + s;
						case _:
							throw "String expected";
					}
				}
			}

			switch (field.kind) {
				case FFun(f) if (f.expr != null):
					f.expr = macro @:pos(f.expr.pos) {
						ctx = new DisplayTestContext($v{filename}, $v{field.name}, $v{transform.source}, $markers);
						static var methodArgs = {method: haxe.display.Protocol.Methods.ResetCache, id: 1, params: {}};
						var args = ['--display', haxe.Json.stringify(methodArgs)];
						ctx.runHaxe(args);
						${f.expr}
					};
				case _:
			}
		}

		return fields;
	}
	#end

	macro static public function getCases(pack:String) {
		var cases = [];
		var singleCase = haxe.macro.Context.definedValue("test");
		function loop(pack:Array<String>) {
			var path = Context.resolvePath(Path.join(pack));
			for (file in sys.FileSystem.readDirectory(path)) {
				if (singleCase != null && !file.endsWith(singleCase + ".hx")) {
					continue;
				}
				if (file.endsWith("import.hx")) {
					continue;
				}
				var p = new haxe.io.Path(file);
				if (p.ext == "hx") {
					var tp = {pack: pack, name: p.file};
					cases.push(macro new $tp());
				} else if (Path.join([path, file]).isDirectory()) {
					loop(pack.concat([file]));
				}
			}
		}
		loop(pack.split('.'));
		return macro $a{cases};
	}
}
