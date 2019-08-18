import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.io.Path;

using StringTools;
using sys.FileSystem;

class Macro {
	static var singleCaseField = null;

	macro static function buildCase():Array<Field> {
		var measureFields = [];
		var fields = Context.getBuildFields();
		for (field in fields) {
			if (field.name.startsWith("measure")) {
				if (singleCaseField == null || field.name.startsWith(singleCaseField)) {
					measureFields.push(macro $i{field.name});
				}
			}
		}
		fields.push((macro class C {
			public function new() {
				this.fieldFunctions = $a{measureFields};
			}
		}).fields[0]);
		return fields;
	}

	macro static public function getCases(pack:String) {
		var cases = [];
		var singleCase = Context.definedValue("test");
		if (singleCase != null) {
			var split = singleCase.split(".");
			singleCase = split[0];
			singleCaseField = split[1];
		}
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
		if(cases.length == 0) {
			Sys.stderr().writeString('${Context.definedValue("test")} not found.\n');
			Sys.exit(1);
		}
		return macro $a{cases};
	}
}