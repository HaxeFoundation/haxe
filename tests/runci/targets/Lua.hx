package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
import haxe.io.*;
using StringTools;

class Lua {
	static var miscLuaDir(get,never):String;
	static inline function get_miscLuaDir() return miscDir + 'lua/';

	static public function getLuaDependencies(){
		switch (systemName){
			case "Linux":
				Linux.requireAptPackages(["libpcre3-dev", "libssl-dev", "libreadline-dev"]);
				runCommand("pip", ["install", "--user", "hererocks"]);
				var pyUserBase = commandResult("python", ["-m", "site", "--user-base"]).stdout.trim();
				addToPATH(Path.join([pyUserBase, "bin"]));
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
		if (!commandSucceed("luarocks", ["show", lib, version])) {
			runCommand("luarocks", ["install",lib, version, server_arg]);
		} else {
			infoMsg('Lua dependency $lib is already installed at version $version');
		}
	}

	static public function run(args:Array<String>) {

		getLuaDependencies();

		changeDirectory(miscLuaDir);
		runCommand("haxe", ["run.hxml"]);

		for (lv in ["-l5.1", "-l5.2", "-l5.3", "-j2.0", "-j2.1" ]){

			var envpath = Sys.getEnv("HOME") + '/lua_env$lv';
			addToPATH(envpath + '/bin');

			if (systemName == "Mac" && lv.startsWith("-j")) continue;
			Sys.println('--------------------');
			Sys.println('Lua Version: $lv');
			runCommand("hererocks", [envpath, lv, "-rlatest", "-i"]);
			trace('path: ' + Sys.getEnv("PATH"));


			runCommand("lua",["-v"]);

			runCommand("luarocks", ["config", "--lua-incdir"]);
			runCommand("luarocks", ["config", "--lua-libdir"]);
			runCommand("luarocks", ["config", "--lua-ver"]);
			runCommand("luarocks", ["config", "--system-config"]);
			runCommand("luarocks", ["config", "--rock-trees"]);

			// Note: don't use a user config
			// runCommand("luarocks", ["config", "--user-config"], false, true);

			installLib("haxe-deps", "0.0.1-2");

			changeDirectory(unitDir);
			runCommand("haxe", ["compile-lua.hxml"].concat(args));
			runCommand("lua", ["bin/unit.lua"]);

			changeDirectory(sysDir);
			runCommand("haxe", ["compile-lua.hxml"].concat(args));
			runCommand("lua", ["bin/lua/sys.lua"]);

			changeDirectory(miscDir + "luaDeadCode/stringReflection");
			runCommand("haxe", ["compile.hxml"]);
		}
	}
}
