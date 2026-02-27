package runci.targets;

import runci.System.*;
import runci.Config.*;
import haxe.io.*;
using StringTools;

class Lua {
	static var useWindowsVcpkg = false;
	static var msys2Path = Sys.getEnv("MSYS2_LOCATION") ?? "C:\\msys64";

	static public function getLuaDependencies(){
		switch (systemName){
			case "Linux":
				Linux.requireAptPackages(["libpcre2-dev", "libssl-dev", "libuv1-dev", "libreadline-dev", "pipx"]);
			case "Mac":
				if (commandSucceed("python3", ["-V"]))
					infoMsg('python3 has already been installed.');
				else
					runNetworkCommand("brew", ["install", "python3"]);

				attemptCommand("brew", ["install", "pcre2"]);
				runCommand("brew", ["install", "openssl"]);
				runCommand("brew", ["install", "libuv"]);
				runCommand("brew", ["install", "pipx"]);
			case "Windows":
				if (sys.FileSystem.exists(msys2Path)) {
					addToPATH('$msys2Path\\usr\\bin');
					addToPATH('$msys2Path\\ucrt64\\bin');
					runCommand(
						"pacman",
						[
							"--noconfirm",
							"-S",
							"--needed",
							"--overwrite",
							"mingw-w64-ucrt-x86_64-gcc",
							"mingw-w64-ucrt-x86_64-openssl",
							"mingw-w64-ucrt-x86_64-pcre2",
							"mingw-w64-ucrt-x86_64-libuv"
						]
					);
				} else if(Sys.getEnv("VCPKG_INSTALLATION_ROOT") != null) {
					useWindowsVcpkg = true;
					runCommand("vcpkg", ["install", "pcre2:x64-windows-release"]);
					runCommand("vcpkg", ["install", "libuv:x64-windows-release"]);
					addToPATH(Path.join([Sys.getEnv("VCPKG_INSTALLATION_ROOT"), 'installed/x64-windows-release/bin']));
				} else {
					failMsg("Running on windows requires msys2 or vcpkg environment");
				}

		}
		if (commandSucceed("hererocks", ["--version"])) {
			infoMsg('hererocks has already been installed.');
		} else {
			runCommand("pipx", ["ensurepath"]);
			runCommand("pipx", ["install", "git+https://github.com/luarocks/hererocks.git"]);
		}
	}

	static function installLib(lib : String, version : String, ?server :String){
		if (!commandSucceed("luarocks", ["show", lib, version])) {
			final args = ["install", lib, version];
			args.push('WITH_SHARED_LIBUV=ON');
			if (systemName == "Windows" && useWindowsVcpkg) {
				args.push('OPENSSL_DIR=C:\\Program Files\\OpenSSL');
				args.push('OPENSSL_LIBDIR=C:\\Program Files\\OpenSSL\\lib\\VC\\x64\\MD');
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

	static function getVersionDefine(hererocksFlag:String) {
		if (hererocksFlag.startsWith("-l")) {
			return ["-D", 'lua-ver=${hererocksFlag.substr(2)}'];
		} else if (hererocksFlag.startsWith("-j")) {
			return ["-D", "lua-jit"];
		} else {
			throw "unknown version";
		}
	}

	static public function run(args:Array<String>) {

		getLuaDependencies();

		for (lv in ["-l5.1", "-l5.2", "-l5.3", "-l5.4", "-l5.5", "-j2.0", "-j@v2.1"]) {
			// luajit 2.0 was missing arm64 support
			if (System.arch == Arm64 && lv == "-j2.0") continue;

			final envpath = getInstallPath() + '/lua_env/lua${lv.replace("@v", "")}';
			addToPATH(envpath + '/bin');

			Sys.println('--------------------');
			Sys.println('Lua Version: $lv');

			final targetFlags = if (systemName == "Windows") ["--target", if (useWindowsVcpkg) "vs" else "mingw"] else [];
			final luaBin = envpath + '/bin/lua' + (systemName == "Windows" ? ".exe" : "");
			if (!sys.FileSystem.exists(luaBin)) {
				runCommand("hererocks", [envpath, lv, "-r@v3.13.0", "-i"].concat(targetFlags));
			} else {
				infoMsg('Lua environment at $envpath already exists, skipping hererocks.');
			}
			trace('path: ' + Sys.getEnv("PATH"));


			runCommand("lua",["-v"]);

			if (systemName == "Windows") {
				if (useWindowsVcpkg) {
					// required for luv build, default is very old
					runCommand("luarocks", ["config", "cmake_generator", "Visual Studio 17 2022"]);
					runCommand("luarocks", ["config", "external_deps_dirs[0]", Path.join([Sys.getEnv("VCPKG_INSTALLATION_ROOT"), 'installed/x64-windows-release'])]);
				} else {
					runCommand("luarocks", ["config", "external_deps_dirs[0]", Path.join([msys2Path, "ucrt64"])]);
				}
			} else if (systemName == "Mac") {
				runCommand("luarocks", ["config", "external_deps_dirs[0]", commandResult("brew", ["--prefix"]).stdout.trim()]);
			}

			runCommand("luarocks", ["config", "--lua-incdir"]);
			runCommand("luarocks", ["config", "--lua-libdir"]);
			runCommand("luarocks", ["config", "--lua-ver"]);
			runCommand("luarocks", ["config", "--system-config"]);
			runCommand("luarocks", ["config", "--rock-trees"]);

			// Note: don't use a user config
			// attemptCommand("luarocks", ["config", "--user-config"]);

			installLib("luasocket", "3.1.0-1");
			installLib("luasec", "1.3.2-1");

			installLib("lrexlib-pcre2", "2.9.1-1");

			//Install bit32 for lua 5.1 (5.3+ uses native bit operators)
			if (lv == "-l5.1")
				installLib("https://raw.githubusercontent.com/lunarmodules/lua-compat-5.3/refs/heads/master/rockspecs/bit32-scm-1.rockspec", "");

			installLib("https://raw.githubusercontent.com/luvit/luv/refs/heads/master/luv-scm-0.rockspec", "");
			installLib("luautf8", "0.2.1-1");

			installLib("https://raw.githubusercontent.com/HaxeFoundation/hx-lua-simdjson/master/hx-lua-simdjson-scm-1.rockspec", "");

			changeDirectory(unitDir);
			for (versionFlags in [[], getVersionDefine(lv)]) {
				runCommand("haxe", ["compile-lua.hxml"].concat(args).concat(versionFlags));
				runCommand("lua", ["bin/unit.lua"]);
			}

			changeDirectory(sysDir);
			runCommand("haxe", args.concat(["compile-lua.hxml"]));
			runSysTest("lua", ["bin/lua/sys.lua"]);

			changeDirectory(getMiscSubDir("lua", "luaDeadCode", "stringReflection"));
			runCommand("haxe", ["compile.hxml"]);

			changeDirectory(getMiscSubDir(""));
			runCommand("haxe", ["run-base.hxml", "--run", "Main", "lua"]);
		}
	}
}
