package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
using StringTools;

class Lua {
	static public function getLuaDependencies(){
		switch (systemName){
			case "Linux":
				Linux.requireAptPackages(["libpcre3-dev"]);
				runCommand("pip", ["install", "--user", "hererocks"]);
			case "Mac": {
				if (commandSucceed("python3", ["-V"]))
					infoMsg('python3 has already been installed.');
				else
					runCommand("brew", ["install", "python3"], true);

				runCommand("brew", ["install", "pcre"], false, true);
				runCommand("pip3", ["install", "hererocks"]);
			}
		}
	}

	static public function installLuaVersionDependencies(lv:String){
		if (lv == "-l5.1"){
			if (!commandSucceed("luarocks", ["show", "luabit"])) {
				runCommand("luarocks", ["install", "luabitop", "1.0.2-3", "--server=https://luarocks.org/dev"]);
			}
		}
		if (!commandSucceed("luarocks", ["show", "lrexlib-pcre"])) {
			runCommand("luarocks", ["install", "lrexlib-pcre", "2.8.0-1", "--server=https://luarocks.org/dev"]);
		}
		if (!commandSucceed("luarocks", ["show", "luv"])) {
			runCommand("luarocks", ["install", "luv", "1.9.1-0", "--server=https://luarocks.org/dev"]);
		}
		if (!commandSucceed("luarocks", ["show", "luasocket"])) {
			runCommand("luarocks", ["install", "luasocket", "3.0rc1-2", "--server=https://luarocks.org/dev"]);
		}
		if (!commandSucceed("luarocks", ["show", "environ"])) {
			runCommand("luarocks", ["install", "environ", "0.1.0-1", "--server=https://luarocks.org/dev"]);
		}
	}

	static public function run(args:Array<String>) {
		getLuaDependencies();
		var envpath = Sys.getEnv("HOME") + '/lua_env';
		addToPATH(envpath + '/bin');
		for (lv in ["-l5.1", "-l5.2", "-l5.3", "-j2.0", "-j2.1" ]){
			if (systemName == "Mac" && lv.startsWith("-j")) continue;
			Sys.println('--------------------');
			Sys.println('Lua Version: $lv');
			runCommand("hererocks", [envpath, lv, "-rlatest", "-i"]);
			trace('path: ' + Sys.getEnv("PATH"));
			runCommand("lua",["-v"]);
			runCommand("luarocks",[]);
			installLuaVersionDependencies(lv);

			changeDirectory(unitDir);
			runCommand("haxe", ["compile-lua.hxml"].concat(args));
			runCommand("lua", ["bin/unit.lua"]);

			changeDirectory(sysDir);
			haxelibInstall("utest");
			runCommand("haxe", ["compile-lua.hxml"].concat(args));
			runCommand("lua", ["bin/lua/sys.lua"]);

			changeDirectory(miscDir + "luaDeadCode/stringReflection");
			runCommand("haxe", ["compile.hxml"]);
		}
	}
}