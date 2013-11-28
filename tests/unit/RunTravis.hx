import sys.io.Process;

class RunTravis {
	static function runProcess(cmd:String, args:Array<String>):Void {
		var p = new Process(cmd, args);
		Sys.println(p.stdout.readAll().toString());
		var exitCode = p.exitCode();
		Sys.println('Process exited with $exitCode: $cmd $args');

		if (exitCode != 0) {
			Sys.exit(1);
		}
	}

	static function main():Void {
		switch (Sys.getEnv("TARGET")) {
			case "macro":
				runProcess("haxe", ["compile-macro.hxml"]);
			case "neko":
				runProcess("haxe", ["compile-neko.hxml"]);
				runProcess("neko", ["unit.n"]);
			case "php":
				runProcess("sudo", ["apt-get", "install", "php", "-y"]);
				runProcess("haxe", ["compile-php.hxml"]);
				runProcess("php", ["php/index.php"]);
			case target:
				throw "unknown target: " + target;
		}
	}
}