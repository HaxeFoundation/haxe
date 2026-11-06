package unit.issues;

import haxe.CallStack;

class Issue12205 extends unit.Test {
	#if jvm
	static var throwLine = 0;

	static function getLine(?p:haxe.PosInfos) {
		return p.lineNumber;
	}

	static function throwing() {
		throwLine = getLine(); throw new haxe.Exception("");
	}

	// CI also runs this with --hxb-lib, where no source files are parsed
	function test() {
		var stack = try {
			throwing();
			null;
		} catch (e:haxe.Exception) {
			e.stack;
		}
		switch (stack[0]) {
			case FilePos(_, file, line, _):
				eq("Issue12205.hx", haxe.io.Path.withoutDirectory(file));
				eq(throwLine, line);
			case item:
				assert('Unexpected stack item: $item');
		}
	}
	#end
}
