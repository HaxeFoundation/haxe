import haxe.macro.Context;
import haxe.macro.Expr;

using StringTools;

/**
	Macro that validates HL bytecode output against @:hl(...) annotations.

	Tests annotate methods with expected HL dump output:
	```haxe
	@:hl(<>
		fun@N(Nh) ():void
		r0 void
		@0 ret 0
	</>)
	static function myTest() {}
	```

	The macro reads `dump/hlcode.txt` after generation and compares (after normalization)
	the expected output with the actual generated HL code for each annotated method.

	Normalization removes unstable elements:
	- Source line numbers (`.12    @0` → `@0`)
	- Source comment lines (`; file:line (Name)`)
	- Function indices (`fun@23(17h)` → `fun@N(Nh)`)
	- Global IDs (replaced by sequential `$0`, `$1`, ... per function)
	- Integer constant pool references (`int R,@19` → `int R,@$N`)
**/
class Macro {
	static var failures = 0;
	static var tests = 0;
	static var classes:Array<String> = [];

	/** Entry point: call this from compile.hxml with `--macro Macro.run()` **/
	public static macro function run():Void {
		Context.onAfterInitMacros(() -> {
			var dir = sys.FileSystem.readDirectory("src/cases");
			for (file in dir) {
				if (file.endsWith(".hx")) {
					var name = "cases." + file.substring(0, file.length - 3);
					Context.getType(name);
					classes.push(name);
				}
			}
		});
		Context.onAfterGenerate(check);
	}

	#if macro
	static function check() {
		var dumpFile = "dump/hlcode.txt";
		if (!sys.FileSystem.exists(dumpFile)) {
			Context.fatalError('$dumpFile not found. Compile with -D dump.', Context.currentPos());
			return;
		}
		var dump = sys.io.File.getContent(dumpFile);
		var functions = parseFunctions(dump);

		for (className in classes) {
			testClass(className, functions);
		}

		trace('Done $tests tests ($failures failures)');
		trace('SUCCESS: ${failures == 0}');
		Sys.exit(failures == 0 ? 0 : 1);
	}

	static function testClass(className:String, functions:Map<String, String>) {
		var c = switch (Context.getType(className)) {
			case TInst(c, _): c.get();
			case _: return;
		};

		function checkField(cf:haxe.macro.Type.ClassField) {
			for (m in cf.meta.get()) {
				if (m.name == ":hl") {
					if (m.params.length == 0) {
						Context.warning('Expected @:hl content', cf.pos);
						return;
					}
					var expected = extractHlContent(m.params[0]);
					var funcName = className + "." + cf.name;
					++tests;
					if (!functions.exists(funcName)) {
						++failures;
						Context.warning('Test failed: could not find function $funcName in HL dump', cf.pos);
						return;
					}
					var actual = functions.get(funcName);
					var normalizedExpected = normalize(expected);
					var normalizedActual = normalize(actual);
					if (normalizedExpected != normalizedActual) {
						++failures;
						Context.warning('Test failed for $funcName\n${makeDiff(normalizedExpected, normalizedActual)}', cf.pos);
					}
				}
			}
		}

		for (cf in c.statics.get()) {
			checkField(cf);
		}
		for (cf in c.fields.get()) {
			checkField(cf);
		}
	}

	static function extractHlContent(e:Expr):String {
		return switch (e.expr) {
			case EConst(CString(s, _)):
				s;
			case EMeta({name: ":markup"}, {expr: EConst(CString(s, _))}):
				// XML literal <> ... </>: strip the outer tags
				var s = s.trim();
				if (s.startsWith("<>") && s.endsWith("</>")) {
					s.substring(2, s.length - 3);
				} else {
					s;
				}
			default:
				Context.error("String or markup literal expected for @:hl", e.pos);
		}
	}

	/**
		Produce a unified diff between two normalized strings for readable test failure output.
	**/
	static function makeDiff(expected:String, actual:String):String {
		final a = new diff.FileData(haxe.io.Bytes.ofString(expected), "expected", Date.now());
		final b = new diff.FileData(haxe.io.Bytes.ofString(actual), "actual", Date.now());
		final ctx:diff.Context = {file1: a, file2: b, context: 3};
		final script = diff.Analyze.diff2Files(ctx);
		final result = diff.Printer.printUnidiff(ctx, script);
		// Strip the file header lines (--- expected / +++ actual)
		return result.split("\n").slice(2).join("\n");
	}

	/**
		Parse the HL dump into a map of `ClassName.methodName` → function block text.
	**/
	static function parseFunctions(dump:String):Map<String, String> {
		var result = new Map<String, String>();
		var lines = dump.replace("\r", "").split("\n");
		var i = 0;
		var inFunctions = false;
		var isSectionEnd = ~/^\d+ (objects protos|constant values)$/;
		var funcNameRegex = ~/\(([^()]+)\)\s*$/;

		while (i < lines.length) {
			var line = lines[i];

			if (!inFunctions) {
				if (~/^\d+ functions$/.match(line)) {
					inFunctions = true;
				}
				i++;
				continue;
			}

			// End of functions section
			if (isSectionEnd.match(line)) {
				break;
			}

			// Function header: "\tfun@N(Nh) type"
			if (line.startsWith("\tfun@")) {
				var funcHeader = line;
				i++;

				// Source comment: "\t; FILE:LINE (Class.Method)"
				if (i < lines.length && lines[i].startsWith("\t;")) {
					var commentLine = lines[i];
					// Extract function name from the last "(...)" in the comment
					var funcName = funcNameRegex.match(commentLine) ? funcNameRegex.matched(1) : null;
					i++;

					// Collect register and instruction lines until next function or section end
					var bodyLines = [funcHeader, commentLine];
					while (i < lines.length) {
						var bodyLine = lines[i];
						if (bodyLine.startsWith("\tfun@") || isSectionEnd.match(bodyLine)) {
							break;
						}
						bodyLines.push(bodyLine);
						i++;
					}

					if (funcName != null) {
						result.set(funcName, bodyLines.join("\n"));
					}
				}
			} else {
				i++;
			}
		}

		return result;
	}

	/**
		Normalize a function block for comparison by removing unstable elements.
	**/
	static function normalize(text:String):String {
		var lines = text.replace("\r", "").split("\n");
		var result = [];

		// Per-function global ID mapping for stable comparison
		var globalMap = new Map<String, String>();
		var globalCounter = 0;

		function getGlobalId(gid:String):String {
			if (!globalMap.exists(gid)) {
				globalMap.set(gid, "$" + globalCounter);
				globalCounter++;
			}
			return globalMap.get(gid);
		}

		for (line in lines) {
			var trimmed = line.trim();
			if (trimmed == "") continue;

			// Skip source comment lines: "; file:line (Name)"
			if (trimmed.startsWith(";")) continue;

			// Normalize function index in declaration: "fun@23(17h) type" → "fun@N(Nh) type"
			trimmed = ~/^fun@\d+\([0-9A-F]+h\)/.replace(trimmed, "fun@N(Nh)");

			// Remove source line prefix from instruction lines: ".12    @0" → "@0"
			trimmed = ~/^\.\d+[ \t]+/.replace(trimmed, "");

			// Normalize dynset constant in "dynset 7[@159],0" → "dynset 7[@0],0"
			trimmed = ~/\bdynset (\d+)\[@(\d+)\],(\d+)/.map(trimmed, function(r) {
				return "dynset " + r.matched(1) + "[@" + getGlobalId("str_" + r.matched(2)) + "]," + r.matched(3);
			});

			// Normalize global IDs in "global R, G" (OGetGlobal): G is the global index
			trimmed = ~/\bglobal (\d+), (\d+)/.map(trimmed, function(r) {
				return "global " + r.matched(1) + ", " + getGlobalId(r.matched(2));
			});

			// Normalize global IDs in "setglobal G, R" (OSetGlobal): G is the global index
			trimmed = ~/\bsetglobal (\d+), (\d+)/.map(trimmed, function(r) {
				return "setglobal " + getGlobalId(r.matched(1)) + ", " + r.matched(2);
			});

			// Normalize integer constant pool references: "int R,@N" → "int R,@I0"
			// The @N references the integer constant pool, which is unstable across compilations
			trimmed = ~/\bint (\d+),@(\d+)/.map(trimmed, function(r) {
				return "int " + r.matched(1) + ",@" + getGlobalId("intpool_" + r.matched(2));
			});

			result.push(trimmed);
		}

		return result.join("\n");
	}
	#end
}
