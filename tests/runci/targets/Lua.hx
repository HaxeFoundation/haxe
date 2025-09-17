package runci.targets;

import runci.System.*;
import runci.Config.*;
import haxe.io.*;
using StringTools;

class Lua {
	static final miscLuaDir = getMiscSubDir('lua');

	static public function getLuaDependencies(){
		switch (systemName){
			case "Linux":
				Linux.requireAptPackages(["libpcre2-dev", "libssl-dev", "libreadline-dev", "pipx"]);
				runCommand("pipx", ["ensurepath"]);
				runCommand("pipx", ["install", "hererocks"]);
			case "Mac": {
				if (commandSucceed("python3", ["-V"]))
					infoMsg('python3 has already been installed.');
				else
					runNetworkCommand("brew", ["install", "python3"]);

				attemptCommand("brew", ["install", "pcre2"]);
				runCommand("brew", ["install", "openssl"]);
				runCommand("brew", ["install", "pipx"]);
				runCommand("pipx", ["ensurepath"]);
				runCommand("pipx", ["install", "hererocks"]);
			}
		}
	}

	static function installLib(lib : String, version : String, ?server :String){
		if (!commandSucceed("luarocks", ["show", lib, version])) {
			final args = ["install", lib, version];
			if (systemName == "Mac") {
				final opensslPath = commandResult("brew", ["--prefix", "openssl"]);
				args.push('OPENSSL_DIR=${opensslPath.stdout.trim()}');
				final pcrePath = commandResult("brew", ["--prefix", "pcre2"]);
				args.push('PCRE2_DIR=${pcrePath.stdout.trim()}');
			}
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

		for (lv in ["-l5.1", "-l5.2", "-l5.3", "-l5.4"].concat(systemName == 'Linux' && Linux.arch == Arm64 ? [] : ["-j2.0", "-j2.1"])) {
			final envpath = getInstallPath() + '/lua_env/lua$lv';
			addToPATH(envpath + '/bin');

			if (systemName == "Mac" && lv.startsWith("-j")) continue;
			Sys.println('--------------------');
			Sys.println('Lua Version: $lv');
			runCommand("hererocks", [envpath, lv, "-r@418d2ab34891b130cc317df32f65f978640febcf", "-i"]);
			trace('path: ' + Sys.getEnv("PATH"));


			runCommand("lua",["-v"]);

			runCommand("luarocks", ["config", "--lua-incdir"]);
			runCommand("luarocks", ["config", "--lua-libdir"]);
			runCommand("luarocks", ["config", "--lua-ver"]);
			runCommand("luarocks", ["config", "--system-config"]);
			runCommand("luarocks", ["config", "--rock-trees"]);

			// Note: don't use a user config
			// attemptCommand("luarocks", ["config", "--user-config"]);

			installLib("luasec", "1.0.2-1");

			installLib("lrexlib-pcre2", "2.9.1-1");
			installLib("luasocket", "3.0rc1-2");

			//Install bit32 for lua 5.1
			if (lv == "-l5.1")
				installLib("bit32", "5.2.2-1");

			installLib("luv", "1.50.0-1");
			if (lv == "-l5.4") {
				installLib("bit32", "5.3.5.1-1");
				installLib("luautf8", "0.1.5-2");
			} else {
				installLib("luautf8", "0.1.1-1");
			}

			installLib("https://raw.githubusercontent.com/HaxeFoundation/hx-lua-simdjson/master/hx-lua-simdjson-scm-1.rockspec", "");

			changeDirectory(unitDir);
			final luaDefine = if (lv.startsWith("-l")) {
				lv.replace("-l", "lua").replace(".", "_");
			} else lv.replace("-j", "luajit").replace(".", "_");
			final luaVer = ["-D", luaDefine];
			runCommand("haxe", ["compile-lua.hxml"].concat(args).concat(luaVer));
			runCommand("lua", ["bin/unit.lua"]);

			Display.maybeRunDisplayTests(Lua);

			changeDirectory(sysDir);
			runCommand("haxe", ["compile-lua.hxml"].concat(args).concat(luaVer));
			runSysTest("lua", ["bin/lua/sys.lua"]);

			changeDirectory(getMiscSubDir("luaDeadCode", "stringReflection"));
			runCommand("haxe", ["compile.hxml"]);

			changeDirectory(miscLuaDir);
			runCommand("haxe", ["run.hxml"]);
		}
	}
}
