package runci.targets;

import runci.System.*;
import runci.Config.*;
import haxe.io.*;
using StringTools;

class Lua {
	static final miscLuaDir = getMiscSubDir('lua');

	static public function getLuaDependencies(){
		switch (systemName){
			case "Windows":
				//choco install openssl
				runCommand("choco", ["install", "openssl"]);
				
				//Try to rename openssl bins for 64 bit
				attemptCommand("cp", ["C:\\Program Files\\OpenSSL-Win64\\bin\\libssl-1_1-x64.dll", "C:\\Program Files\\OpenSSL-Win64\\bin\\libssl32MD.dll"]);
				attemptCommand("cp", ["C:\\Program Files\\OpenSSL-Win64\\bin\\libcrypto-1_1-x64.dll", "C:\\Program Files\\OpenSSL-Win64\\bin\\libcrypto32MD.dll"]);
				
				//Try to rename openssl bins for 32 bit
				attemptCommand("cp", ["C:\\Program Files\\OpenSSL-Win64\\bin\\libssl-1_1.dll", "C:\\Program Files\\OpenSSL-Win64\\bin\\libssl32MD.dll"]);
				attemptCommand("cp", ["C:\\Program Files\\OpenSSL-Win64\\bin\\libcrypto-1_1.dll", "C:\\Program Files\\OpenSSL-Win64\\bin\\libcrypto32MD.dll"]);
				
				//Install libs for pcre
				runCommand("choco", ["install", "msys2"]);
				
				//Invoke pacman from msys2 to install pcre
				//attemptCommand("C:\\tools\\msys64\\usr\\bin\\bash.exe", ["-lc", "pacman", "-S", "mingw-w64-x86_64-pcre"]);
				attemptCommand("C:\\tools\\msys64\\usr\\bin\\pacman.exe", ["-S", "--noconfirm", "mingw-w64-x86_64-pcre"]);
				
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
				runCommand("brew", ["install", "openssl"]);
			}
		}
	}

	static function installLib(lib : String, version : String, ?server :String){
		if (!commandSucceed("luarocks", ["show", lib, version])) {
            final args = ["install", lib, version];
			if (systemName == "Mac") {
				args.push('OPENSSL_DIR=/usr/local/opt/openssl@3');
			}
			if (systemName == "Windows") {
				args.push('OPENSSL_DIR=C:\\Program Files\\OpenSSL-Win64');	
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

		for (lv in ["-l5.1", "-l5.2", "-l5.3"].concat(systemName == 'Linux' && Linux.arch == Arm64 ? [] : ["-j2.0", "-j2.1"])) {
			final envpath = getInstallPath() + '/lua_env/lua$lv';
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

			installLib("luasec", "1.0.2-1");
			
			installLib("lrexlib-pcre", "2.9.1-1");
			installLib("luv", "1.36.0-0");
			installLib("luasocket", "3.0rc1-2");
			installLib("luautf8", "0.1.1-1");
			
			//Install bit32 for lua 5.1
			if(lv == "-l5.1"){
				installLib("bit32", "5.2.2-1");
			}
			
			installLib("hx-lua-simdjson", "0.0.1-1");
			
			changeDirectory(unitDir);
			runCommand("haxe", ["compile-lua.hxml"].concat(args));
			runCommand("lua", ["bin/unit.lua"]);

			changeDirectory(sysDir);
			runCommand("haxe", ["compile-lua.hxml"].concat(args));
			runSysTest("lua", ["bin/lua/sys.lua"]);

			changeDirectory(getMiscSubDir("luaDeadCode", "stringReflection"));
			runCommand("haxe", ["compile.hxml"]);

			changeDirectory(miscLuaDir);
			runCommand("haxe", ["run.hxml"]);
		}
	}
}
