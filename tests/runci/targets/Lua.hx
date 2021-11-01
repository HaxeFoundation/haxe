package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
import haxe.io.*;
using StringTools;

class Lua {
	static final miscLuaDir = miscDir + 'lua/';

	static public function getLuaDependencies(){
		switch (systemName){
			case "Linux":
				Linux.requireAptPackages(["libpcre3-dev", "libssl-dev", "libreadline-dev"]);
				runCommand("pip", ["install", "--user", "hererocks"]);
				final pyUserBase = commandResult("python", ["-m", "site", "--user-base"]).stdout.trim();
				addToPATH(Path.join([pyUserBase, "bin"]));
			case "Mac": {
				if (commandSucceed("python3", ["-V"]))
					infoMsg('python3 has already been installed.');
				else
					runNetworkCommand("brew", ["install", "python3"]);

				attemptCommand("brew", ["install", "pcre"]);
				runCommand("pip3", ["install", "hererocks"]);
			}
		}
	}

	static function installLib(lib : String, version : String, ?server :String){
		if (!commandSucceed("luarocks", ["show", lib, version])) {
            final args = ["install", lib, version];
            if (server != null){
                final server_arg = '--server=$server';
                args.push(server_arg);
            }
			runCommand("luarocks", args);
		} else {
			infoMsg('Lua dependency $lib is already installed at version $version');
		}
	}

	static public function run(args:Array<String>) {

		getLuaDependencies();

		for (lv in ["-l5.1", "-l5.2", "-l5.3"].concat(systemName == 'Linux' && Linux.arch == Arm64 ? [] : ["-j2.0", "-j2.1"])) {

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
			// attemptCommand("luarocks", ["config", "--user-config"]);

			installLib("haxe-deps", "0.0.1-6");

			changeDirectory(unitDir);
			runCommand("haxe", ["compile-lua.hxml"].concat(args));
			runCommand("lua", ["bin/unit.lua"]);

			changeDirectory(sysDir);
			runCommand("haxe", ["compile-lua.hxml"].concat(args));
			runSysTest("lua", ["bin/lua/sys.lua"]);

			changeDirectory(miscDir + "luaDeadCode/stringReflection");
			runCommand("haxe", ["compile.hxml"]);

			changeDirectory(miscLuaDir);
			runCommand("haxe", ["run.hxml"]);
		}
	}
}
