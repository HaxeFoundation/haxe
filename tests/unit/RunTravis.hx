import sys.io.Process;

class RunTravis {
	static function runProcess(p:Process):Void {
		Sys.println(p.stdout.readAll().toString());

		if (p.exitCode() != 0) {
			Sys.exit(1);
		}
	}

	static function main():Void {
		switch (Sys.getEnv("TARGET")) {
			case "macro":
				runProcess(new Process("haxe", ["compile-macro.hxml"]));
			case "neko":
				runProcess(new Process("haxe", ["compile-neko.hxml"]));
				runProcess(new Process("neko", ["unit.n"]));
			case target:
				throw "unknown target: " + target;
		}
	}
}