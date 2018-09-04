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

	static function installLib(lib : String, version : String, server = "https://luarocks.org/dev"){
		var server_arg = '--server=$server';
		if (!commandSucceed("luarocks", ["show", lib])) {
			runCommand("luarocks", ["install",lib, version, server_arg]);
		}
	}

	static public function installLuaVersionDependencies(lv:String){
		if (lv == "-l5.1") installLib("luabitop", "1.0.2-3");

		installLib("lrexlib-pcre" , "2.8.0-1");
		installLib("luv"          , "1.22.0-1");
		installLib("luasocket"    , "3.0rc1-2");
		installLib("environ"      , "0.1.0-1");
		installLib("luautf8"      , "0.1.1-1");

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
