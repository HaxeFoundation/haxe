import sys.io.Process;

class RunTravis {
	static function runProcess(cmd:String, args:Array<String>):Void {
		var p = new Process(cmd, args);
		Sys.println(p.stdout.readAll().toString());
		Sys.println(p.stderr.readAll().toString());

		var exitCode = p.exitCode();
		Sys.println('Process exited with $exitCode: $cmd $args');

		if (exitCode != 0) {
			Sys.exit(1);
		}
	}

	static function main():Void {
		var cwd = Sys.getCwd();
		switch (Sys.getEnv("TARGET")) {
			case "macro":
				runProcess("haxe", ["compile-macro.hxml"]);
			case "neko":
				runProcess("haxe", ["compile-neko.hxml"]);
				runProcess("neko", ["unit.n"]);
			case "php":
				runProcess("sudo", ["apt-get", "install", "php5", "-y"]);
				runProcess("haxe", ["compile-php.hxml"]);
				runProcess("php", ["php/index.php"]);
			case "cpp":
				//hxcpp dependencies
				runProcess("sudo", ["apt-get", "install", "gcc-multilib", "g++-multilib", "-y"]);

				//install and build hxcpp
				runProcess("haxelib", ["git", "hxcpp", "https://github.com/HaxeFoundation/hxcpp.git"]);
				Sys.setCwd(Sys.getEnv("HOME") + "/haxelib/hxcpp/git/runtime/");
				runProcess("haxelib", ["run", "hxcpp", "BuildLibs.xml"]);
				Sys.setCwd(cwd);
				
				runProcess("haxe", ["compile-cpp.hxml"]);
				runProcess("./cpp/Test-debug", []);
			case target:
				throw "unknown target: " + target;
		}
	}
}