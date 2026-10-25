import cases.*;

typedef Case = {
	name:String,
	run:() -> Int,
	reps:Int
}

/**
	Benchmarks of the code that macros run, for the eval interpreter.

	The same cases run with `--interp` (`interp.hxml`), as a macro
	(`macro.hxml`) and on node (`js.hxml`). Each case returns a checksum, which
	must be the same in every mode.

	Arguments (`-D bench-filter=<name>` for the macro mode): a case name runs
	only that case.
**/
class Main {
	static final cases:Array<Case> = [
		{name: "calls", run: Calls.run, reps: 10},
		{name: "objects", run: Objects.run, reps: 12},
		{name: "arrays", run: Arrays.run, reps: 6},
		{name: "anon", run: Anon.run, reps: 40},
		{name: "closures", run: Closures.run, reps: 20},
		{name: "strings", run: Strings.run, reps: 6},
		{name: "maps", run: Maps.run, reps: 10},
		{name: "enums", run: Enums.run, reps: 150},
		{name: "shader", run: Shader.run, reps: 2},
	];

	static function main() {
		#if sys
		var args = Sys.args();
		#elseif js
		var args:Array<String> = js.Syntax.code("process.argv.slice(2)");
		#else
		var args = [];
		#end
		runAll(args.length > 0 ? args[0] : null);
	}

	#if macro
	static function runMacro() {
		runAll(haxe.macro.Context.definedValue("bench-filter"));
	}
	#end

	static function runAll(filter:Null<String>) {
		for (c in cases) {
			if (filter != null && filter != c.name)
				continue;
			var t0 = stamp();
			var sum = 0;
			for (_ in 0...c.reps)
				sum = (sum * 31 + c.run()) & 0xFFFFFF;
			var t = stamp() - t0;
			println('${c.name}\t${Math.round(t * 1000)}\t$sum');
		}
	}

	static function stamp() {
		#if (sys || macro)
		return Sys.time();
		#else
		return haxe.Timer.stamp();
		#end
	}

	static function println(msg:String) {
		#if (sys || macro)
		Sys.println(msg);
		#elseif js
		js.Syntax.code('console.log({0})', msg);
		#else
		trace(msg);
		#end
	}
}
