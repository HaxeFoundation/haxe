using StringTools;

class ParseError extends haxe.Exception {}

enum FileOp {
	Write(path:String, content:String);
	Delete(path:String);
	Overlay(path:String, content:String);
	RemoveOverlay(path:String);
}

enum Expectation {
	Success;
	Error(text:String);
	Warning(text:String);
	NoWarning;
	NoFsCalls;
	Output(text:String);
	CompilerOutput(text:String);
	NoOutput(text:String);
	FileContains(path:String, text:String);
	FileNotContains(path:String, text:String);
}

typedef Step = {
	name:String,
	args:Array<String>,
	ops:Array<FileOp>,
	expectations:Array<Expectation>
}

/**
	Parses `.hxtest` files. Format:

	```
	# Free text description (lines starting with `#` before the first section).
	@targets interp js          header directives
	@args -D foo                extra compiler arguments for every step
	@main Main                  main class (`none` for no `-main`)
	@known-failure <reason>     the case is expected to fail for now
	@known-failure [t1,t2] <r>  only on the given targets
	@real-mtime                 do not give each step its own modification time

	=== initial                 first section: the initial state
	@expect-success             step directives come first in a section
	--- Main.hx                 then file blocks: content until the next
	class Main { ... }          `--- ` or `=== ` line
	--- delete Old.hx           removes a file
	--- overlay Foo.hx          unsaved editor content (server/overlay); the
	                            cold compilation sees it as the file content
	--- remove-overlay Foo.hx   drops the overlay

	=== change something        every following section is an edit step
	@args -D bar                replaces the extra arguments from this step on
	@expect-output hello
	--- Main.hx
	...
	```

	Step expectations: `@expect-success`, `@expect-error <text>`,
	`@expect-warning <text>`, `@expect-no-warning`, `@expect-no-fs-calls`, `@expect-output <text>`,
	`@expect-no-output <text>`, `@expect-compiler-output <text>`,
	`@expect-file <path> <text>`,
	`@expect-no-file-content <path> <text>`.
	Program output is the `--interp` output, or the output of running
	`out/main.js` with node for the js target.
**/
class CaseParser {
	public static function parse(name:String, source:String):TestCase {
		var tc = new TestCase(name);
		var lines = source.replace("\r\n", "\n").split("\n");
		var i = 0;
		var currentArgs:Array<String> = [];

		// Header.
		while (i < lines.length && !lines[i].startsWith("=== ")) {
			var line = lines[i].trim();
			if (line.startsWith("@")) {
				var d = directive(line);
				switch (d.name) {
					case "targets":
						tc.targets = d.rest.split(" ").filter(s -> s != "");
					case "args":
						currentArgs = splitArgs(d.rest);
					case "main":
						tc.main = d.rest == "none" ? null : d.rest;
					case "real-mtime":
						tc.realMtime = true;
					case "known-failure":
						var rest = d.rest;
						if (rest.startsWith("[")) {
							var end = rest.indexOf("]");
							tc.knownFailureTargets = rest.substring(1, end).split(",").map(StringTools.trim);
							rest = rest.substr(end + 1).trim();
						}
						tc.knownFailure = rest == "" ? "known failure" : rest;
					case other:
						throw new ParseError('$name: unknown header directive @$other');
				}
			} else if (line != "" && !line.startsWith("#")) {
				throw new ParseError('$name: unexpected header line "$line"');
			}
			i++;
		}

		while (i < lines.length) {
			var step:Step = {
				name: lines[i].substr(4).trim(),
				args: currentArgs,
				ops: [],
				expectations: []
			};
			i++;
			// Step directives.
			while (i < lines.length && !lines[i].startsWith("--- ") && !lines[i].startsWith("=== ")) {
				var line = lines[i].trim();
				if (line.startsWith("@")) {
					var d = directive(line);
					switch (d.name) {
						case "args":
							currentArgs = splitArgs(d.rest);
							step.args = currentArgs;
						case "expect-success":
							step.expectations.push(Success);
						case "expect-error":
							step.expectations.push(Error(d.rest));
						case "expect-warning":
							step.expectations.push(Warning(d.rest));
						case "expect-no-warning":
							step.expectations.push(NoWarning);
						case "expect-no-fs-calls":
							step.expectations.push(NoFsCalls);
						case "expect-output":
							step.expectations.push(Output(d.rest));
						case "expect-compiler-output":
							step.expectations.push(CompilerOutput(d.rest));
						case "expect-no-output":
							step.expectations.push(NoOutput(d.rest));
						case "expect-file":
							var p = firstWord(d.rest);
							step.expectations.push(FileContains(p.word, p.rest));
						case "expect-no-file-content":
							var p = firstWord(d.rest);
							step.expectations.push(FileNotContains(p.word, p.rest));
						case other:
							throw new ParseError('$name: unknown step directive @$other in step "${step.name}"');
					}
				} else if (line != "" && !line.startsWith("#")) {
					throw new ParseError('$name: unexpected line "$line" in step "${step.name}"');
				}
				i++;
			}
			// File blocks.
			while (i < lines.length && lines[i].startsWith("--- ")) {
				var header = lines[i].substr(4).trim();
				i++;
				var content = [];
				while (i < lines.length && !lines[i].startsWith("--- ") && !lines[i].startsWith("=== ")) {
					content.push(lines[i]);
					i++;
				}
				if (header.startsWith("delete ")) {
					step.ops.push(Delete(header.substr(7).trim()));
				} else if (header.startsWith("remove-overlay ")) {
					step.ops.push(RemoveOverlay(header.substr(15).trim()));
				} else if (header.startsWith("overlay ")) {
					while (content.length > 0 && content[content.length - 1].trim() == "")
						content.pop();
					step.ops.push(Overlay(header.substr(8).trim(), content.join("\n") + "\n"));
				} else {
					while (content.length > 0 && content[content.length - 1].trim() == "")
						content.pop();
					step.ops.push(Write(header, content.join("\n") + "\n"));
				}
			}
			tc.steps.push(step);
		}
		if (tc.steps.length == 0)
			throw new ParseError('$name: no steps');
		return tc;
	}

	static function directive(line:String) {
		var p = firstWord(line.substr(1));
		return {name: p.word, rest: p.rest};
	}

	static function firstWord(s:String) {
		s = s.trim();
		var sp = s.indexOf(" ");
		return sp < 0 ? {word: s, rest: ""} : {word: s.substr(0, sp), rest: s.substr(sp + 1).trim()};
	}

	static function splitArgs(s:String) {
		return s.split(" ").filter(a -> a != "");
	}
}

class TestCase {
	public var name:String;
	public var targets:Array<String> = ["interp", "js"];
	public var main:Null<String> = "Main";
	public var knownFailure:Null<String> = null;
	public var realMtime:Bool = false;
	/** Targets the known failure applies to, all targets if null. **/
	public var knownFailureTargets:Null<Array<String>> = null;

	public function knownFailureFor(target:String):Null<String> {
		return knownFailureTargets == null || knownFailureTargets.contains(target) ? knownFailure : null;
	}
	public var steps:Array<Step> = [];

	public function new(name:String) {
		this.name = name;
	}

	public function hxmlFor(step:Step, target:String) {
		var lines = [];
		if (main != null)
			lines.push('-main $main');
		switch (target) {
			case "interp":
				lines.push("--interp");
			case "js":
				lines.push("-js out/main.js");
			case "hl":
				lines.push("--hl out/main.hl");
			case other:
				throw new ParseError('$name: unknown target $other');
		}
		for (a in step.args)
			lines.push(a);
		return lines.join("\n") + "\n";
	}
}
