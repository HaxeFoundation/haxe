import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;

using StringTools;

class Macro {
	static var classes = [];
	static var output;
	static var lines;
	static var tests = 0;
	static var failures = 0;

	static function register(className:String) {
		if (classes.length == 0) {
			Context.onAfterGenerate(run);
		}
		Context.getType(className);
		classes.push(className);
	}

	static function run() {
		output = sys.io.File.getContent(haxe.macro.Compiler.getOutput());
		lines = output.replace("\r", "").split("\n");
		for (className in classes) {
			test(className);
		}
		trace('Done $tests tests ($failures failures)');
		trace("SUCCESS: " + (failures == 0));
		Sys.exit(failures == 0 ? 0 : 1);
	}

	static function test(className:String) {
		var c = switch(Context.getType(className)) {
			case TInst(c, _): c.get();
			case _: Context.error('$className should be a class', Context.currentPos());
		}
		var fields = [];
		function checkField(cf:ClassField) {
			if (cf.meta.has(":js")) {
				fields.push({name: cf.name, js: extractJs(cf.meta.get()), pos: cf.pos});
			}
		}
		for (cf in c.statics.get()) {
			checkField(cf);
		}
		for (field in fields) {
			var name = '$className.${field.name}';
			var output = getOutput(name);
			++tests;
			if (output != field.js) {
				++failures;
				Context.warning('$output should be ${field.js}', field.pos);
			}
		}
	}

	static function stripWhitespaces(s:String) {
		return ~/[\r\n\t]/g.replace(s, "");
	}

	static function extractJs(meta:Metadata) {
		for (m in meta) {
			if (m.name == ":js") {
				switch(m.params[0]) {
					case macro $v{(s:String)}: return stripWhitespaces(s);
					case e: Context.error("String expected", e.pos);
				}
			}
		}
		throw false;
	}

	static function getOutput(identifier:String) {
		var buf = new StringBuf();
		for (i in 0...lines.length) {
			if (lines[i].startsWith(identifier)) {
				for (k in (i + 1)...lines.length) {
					if (lines[k].startsWith("\t")) {
						buf.add(lines[k].trim());
					} else {
						return buf.toString();
					}
				}
			}
		}
		return Context.error('Could not find $identifier in output', Context.currentPos());
	}
}