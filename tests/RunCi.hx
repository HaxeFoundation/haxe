using StringTools;

import yaml.*;

import sys.*;
import sys.io.*;
import haxe.*;
import haxe.io.*;

private typedef TravisConfig = {
	before_install: Array<String>,
	script: Array<String>
}

/**
	List of "TEST" defined in the "matrix" section of ".travis.yml".
*/
@:enum abstract TEST(String) from String {
	var Macro = "macro";
	var Neko = "neko";
	var Js = "js";
	var Lua = "lua";
	var Php = "php";
	var Cpp = "cpp";
	var Flash9 = "flash9";
	var As3 = "as3";
	var Java = "java";
	var Cs = "cs";
	var Python = "python";
	var Hl = "hl";
	var ThirdParty = "third-party";
}

enum Ci {
	TravisCI;
	AppVeyor;
}

enum Failure {
	Fail;
}

/**
	Will be run by CI services, currently TravisCI and AppVeyor.

	TravisCI:
	Setting file: ".travis.yml".
	Build result: https://travis-ci.org/HaxeFoundation/haxe

	AppVeyor:
	Setting file: "appveyor.yml".
	Build result: https://ci.appveyor.com/project/HaxeFoundation/haxe
*/
class RunCi {
	static function successMsg(msg:String):Void {
		Sys.println('\x1b[32m' + msg + '\x1b[0m');
	}
	static function failMsg(msg:String):Void {
		Sys.println('\x1b[31m' + msg + '\x1b[0m');
	}
	static function infoMsg(msg:String):Void {
		Sys.println('\x1b[36m' + msg + '\x1b[0m');
	}

	static function fail():Void {
		success = false;
		throw Fail;
	}

	/**
		Run a command using `Sys.command()`.
		If the command exits with non-zero code, exit the whole script with the same code.

		If `useRetry` is `true`, the command will be re-run if it exits with non-zero code (3 trials).
		It is useful for running network-dependent commands.
	*/
	static function runCommand(cmd:String, ?args:Array<String>, useRetry:Bool = false):Void {
		var trials = useRetry ? 3 : 1;
		var exitCode:Int = 1;
		var cmdStr = cmd + (args == null ? '' : ' $args');

		while (trials-->0) {
			Sys.println('Command: $cmdStr');

			var t = Timer.stamp();
			exitCode = Sys.command(cmd, args);
			var dt = Math.round(Timer.stamp() - t);

			if (exitCode == 0)
				successMsg('Command exited with $exitCode in ${dt}s: $cmdStr');
			else
				failMsg('Command exited with $exitCode in ${dt}s: $cmdStr');

			if (exitCode == 0) {
				return;
			} else if (trials > 0) {
				Sys.println('Command will be re-run...');
			}
		}

		fail();
	}

	static function isAptPackageInstalled(aptPackage:String):Bool {
		return commandSucceed("dpkg-query", ["-W", "-f='${Status}'", aptPackage]);
	}

	static function requireAptPackages(packages:Array<String>):Void {
		var notYetInstalled = [for (p in packages) if (!isAptPackageInstalled(p)) p];
		if (notYetInstalled.length > 0)
			runCommand("sudo", ["apt-get", "install", "-y"].concat(notYetInstalled), true);
	}

	static function haxelibInstallGit(account:String, repository:String, ?branch:String, ?srcPath:String, useRetry:Bool = false, ?altName:String):Void {
		var name:String = (altName == null) ? repository : altName;
		try {
			getHaxelibPath(name);
			infoMsg('$name has already been installed.');
		} catch (e:Dynamic) {
			var args:Array<String> = ["git", name, 'https://github.com/$account/$repository'];
			if (branch != null) {
				args.push(branch);
			}
			if (srcPath != null) {
				args.push(srcPath);
			}

			runCommand("haxelib", args, useRetry);
		}
	}

	static function haxelibInstall(library:String):Void {
		try {
			getHaxelibPath(library);
			infoMsg('$library has already been installed.');
		} catch (e:Dynamic) {
			runCommand("haxelib", ["install", library]);
		}
	}

	static function haxelibRun(args:Array<String>, useRetry:Bool = false):Void {
		runCommand("haxelib", ["run"].concat(args), useRetry);
	}

	static function getHaxelibPath(libName:String) {
		var proc = new Process("haxelib", ["path", libName]);
		var result;
		var code = proc.exitCode();
		do {
			result = proc.stdout.readLine();
			if (!result.startsWith("-L")) {
				break;
			}
		} while(true);
		proc.close();
		if (code != 0) {
			throw 'Failed to get haxelib path ($result)';
		}
		return result;
	}

	static function changeDirectory(path:String) {
		Sys.println('Changing directory to $path');
		Sys.setCwd(path);
	}

	static function setupFlashPlayerDebugger():Void {
		var mmcfgPath = switch (systemName) {
			case "Linux":
				Sys.getEnv("HOME") + "/mm.cfg";
			case "Mac":
				"/Library/Application Support/Macromedia/mm.cfg";
			case _:
				throw "unsupported system";
		}

		switch (systemName) {
			case "Linux":
				requireAptPackages([
					"libcurl3:i386", "libglib2.0-0:i386", "libx11-6:i386", "libxext6:i386",
					"libxt6:i386", "libxcursor1:i386", "libnss3:i386", "libgtk2.0-0:i386"
				]);
				runCommand("wget", ["-nv", "http://fpdownload.macromedia.com/pub/flashplayer/updaters/11/flashplayer_11_sa_debug.i386.tar.gz"], true);
				runCommand("tar", ["-xf", "flashplayer_11_sa_debug.i386.tar.gz", "-C", Sys.getEnv("HOME")]);
				File.saveContent(mmcfgPath, "ErrorReportingEnable=1\nTraceOutputFileEnable=1");
				runCommand(Sys.getEnv("HOME") + "/flashplayerdebugger", ["-v"]);
			case "Mac":
				runCommand("brew", ["cask", "install", "flash-player-debugger"]);
				var dir = Path.directory(mmcfgPath);
				runCommand("sudo", ["mkdir", "-p", dir]);
				runCommand("sudo", ["chmod", "a+w", dir]);
				File.saveContent(mmcfgPath, "ErrorReportingEnable=1\nTraceOutputFileEnable=1");
		}
	}

	/**
		Run a Flash swf file.
		Return whether the test is successful or not.
		It detemines the test result by reading the flashlog.txt, looking for "SUCCESS: true".
	*/
	static function runFlash(swf:String):Bool {
		swf = FileSystem.fullPath(swf);
		Sys.println('going to run $swf');
		switch (systemName) {
			case "Linux":
				new Process(Sys.getEnv("HOME") + "/flashplayerdebugger", [swf]);
			case "Mac":
				Sys.command("open", ["-a", "/Applications/Flash Player Debugger.app", swf]);
		}

		//wait a little until flashlog.txt is created
		var flashlogPath = switch (systemName) {
			case "Linux":
				Sys.getEnv("HOME") + "/.macromedia/Flash_Player/Logs/flashlog.txt";
			case "Mac":
				Sys.getEnv("HOME") + "/Library/Preferences/Macromedia/Flash Player/Logs/flashlog.txt";
			case _:
				throw "unsupported system";
		}

		for (t in 0...5) {
			runCommand("sleep", ["2"]);
			if (FileSystem.exists(flashlogPath))
				break;
		}
		if (!FileSystem.exists(flashlogPath)) {
			failMsg('$flashlogPath not found.');
			return false;
		}

		//read flashlog.txt continously
		var traceProcess = new Process("tail", ["-f", flashlogPath]);
		var line = "";
		while (true) {
			try {
				line = traceProcess.stdout.readLine();
				Sys.println(line);
				if (line.indexOf("SUCCESS: ") >= 0) {
					return line.indexOf("SUCCESS: true") >= 0;
				}
			} catch (e:haxe.io.Eof) {
				break;
			}
		}
		return false;
	}

	static function runCs(exe:String, ?args:Array<String>):Void {
		if (args == null) args = [];
		exe = FileSystem.fullPath(exe);
		switch (systemName) {
			case "Linux", "Mac":
				runCommand("mono", [exe].concat(args));
			case "Windows":
				runCommand(exe, args);
				switch (ci) {
					case AppVeyor:
						// https://github.com/HaxeFoundation/haxe/issues/4873
						// runCommand("mono", [exe].concat(args));
					case _:
						//pass
				}
		}
	}

	static function runCpp(bin:String, ?args:Array<String>):Void {
		if (args == null) args = [];
		bin = FileSystem.fullPath(bin);
		runCommand(bin, args);
	}

	static function parseCommand(cmd:String) {
		var args = [];
		var offset = 0;
		var cur = new StringBuf();
		var inString = false;

		while(true) {
			switch(cmd.fastCodeAt(offset++)) {
				case '"'.code:
					inString = !inString;
				case ' '.code if (!inString):
					if (cur.length > 0) {
						args.push(cur.toString());
						cur = new StringBuf();
					}
				case '\\'.code:
					cur.addChar(cmd.fastCodeAt(offset++));
				case "$".code:
					switch (cmd.fastCodeAt(offset)) {
						case '('.code:
							++offset;
							var env = new StringBuf();
							while(true) {
								switch(cmd.fastCodeAt(offset++)) {
									case ')'.code:
										break;
									case c:
										env.addChar(c);
								}
							}
							cur.add(Sys.getEnv(env.toString()));
						case _:
							cur.addChar("$".code);
					}
				case c:
					cur.addChar(c);
			}
			if (offset == cmd.length) {
				break;
			}
		}
		if (cur.length > 0) {
			args.push(cur.toString());
		}
		return args;
	}

	//static function parseTravisFile(path:String, ignoreBeforeInstall = false) {
		//var yaml:TravisConfig = yaml.Yaml.read(path, Parser.options().useObjects());
		//if (!ignoreBeforeInstall) {
			//for (code in yaml.before_install) {
				//var args = parseCommand(code);
				//var cmd = args.shift();
				//runCommand(cmd, args);
			//}
		//}
		//for (code in yaml.script) {
			//var args = parseCommand(code);
			//var cmd = args.shift();
			//runCommand(cmd, args);
		//}
	//}

	static function commandSucceed(cmd:String, args:Array<String>):Bool {
		return try {
			var p = new Process(cmd, args);
			var succeed = p.exitCode() == 0;
			p.close();
			succeed;
		} catch(e:Dynamic) false;
	}

	static function commandResult(cmd:String, args:Array<String>):{
		stdout:String,
		stderr:String,
		exitCode:Int
	} {
		var p = new Process(cmd, args);
		var out = {
			stdout: p.stdout.readAll().toString(),
			stderr: p.stderr.readAll().toString(),
			exitCode: p.exitCode()
		}
		p.close();
		return out;
	}

	static function addToPATH(path:String):Void {
		switch (systemName) {
			case "Windows":
				Sys.putEnv("PATH", Sys.getEnv("PATH") + ";" + path);
			case "Mac", "Linux":
				Sys.putEnv("PATH", Sys.getEnv("PATH") + ":" + path);
		}
	}

	static function getPhpDependencies() {
		switch (systemName) {
			case "Linux":
				if (commandSucceed("php", ["-v"])) {
					infoMsg('php has already been installed.');
				} else {
					requireAptPackages(["php5-cli", "php5-mysql", "php5-sqlite"]);
				}
			case "Mac":
				//pass
			case "Windows":
				if (commandSucceed("php", ["-v"])) {
					infoMsg('php has already been installed.');
				} else {
					runCommand("cinst", ["php", "-version", "5.6.3", "-y"], true);
					addToPATH("C:\\tools\\php");
				}
		}
		runCommand("php", ["-v"]);
	}

	static var gotCppDependencies = false;
	static function getCppDependencies() {
		if (gotCppDependencies) return;

		//hxcpp dependencies
		switch (systemName) {
			case "Linux":
				requireAptPackages(["gcc-multilib", "g++-multilib"]);
			case "Mac":
				//pass
		}


		//install and build hxcpp
		try {
			getHaxelibPath("hxcpp");
			infoMsg('hxcpp has already been installed.');
		} catch(e:Dynamic) {
			haxelibInstallGit("HaxeFoundation", "hxcpp", true);
			var oldDir = Sys.getCwd();
			changeDirectory(getHaxelibPath("hxcpp") + "tools/hxcpp/");
			runCommand("haxe", ["compile.hxml"]);
			changeDirectory(oldDir);
		}

		gotCppDependencies = true;
	}

	static function getJavaDependencies() {
		haxelibInstallGit("HaxeFoundation", "hxjava", true);

		runCommand("javac", ["-version"]);
	}

	static function getJSDependencies() {
		switch (systemName) {
			case "Linux":
				if (commandSucceed("node", ["-v"])) {
					infoMsg('node has already been installed.');
				} else {
					requireAptPackages(["nodejs"]);
				}
			case "Mac":
				//pass
		}

		runCommand("node", ["-v"]);
	}

	static function getLuaDependencies(jit = false, lua_version = "lua5.2", luarocks_version = "2.3.0") {
		switch (systemName){
			case "Linux": requireAptPackages(["libpcre3-dev"]);
			case "Mac": runCommand("brew", ["install", "pcre"]);
		}

		var home_dir = Sys.getEnv("HOME");

		// the lua paths created by the setup script.
		addToPATH('$home_dir/.lua');
		addToPATH('$home_dir/.local/bin');

		// we need to cd back into the build directory to do some work
		var build_dir = Sys.getEnv("TRAVIS_BUILD_DIR");
		changeDirectory(build_dir);

		// luarocks needs to be in the path
		addToPATH('$build_dir/install/luarocks/bin');


		if (jit) Sys.putEnv("LUAJIT","yes");
		Sys.putEnv("LUAROCKS", luarocks_version);
		Sys.putEnv("LUA", lua_version);

		// use the helper scripts in .travis. TODO: Refactor as pure haxe?
		runCommand("sh", ['${build_dir}/.travis/setenv_lua.sh']);
		if (jit){
			runCommand("luajit", ["-v"]);
		} else {
			runCommand("lua", ["-v"]);
		}
		runCommand("pip", ["install", "--user", "cpp-coveralls"]);
		runCommand("luarocks", ["install", "lrexlib-pcre", "2.7.2-1", "--server=https://luarocks.org/dev"]);
		runCommand("luarocks", ["install", "luautf8", "--server=https://luarocks.org/dev"]);

		// we did user land installs of luarocks and lua.  We need to point lua
		// to the luarocks install using the luarocks path and env variables
		var lua_path = commandResult("luarocks", ["path", "--lr-path"]).stdout.trim();
		Sys.putEnv("LUA_PATH", lua_path);
		trace(lua_path + " is the value for lua_path");

		// step two of the variable setting
		var lua_cpath = commandResult("luarocks", ["path", "--lr-cpath"]).stdout.trim();
		Sys.putEnv("LUA_CPATH", lua_cpath);
		trace(lua_cpath + " is the value for lua_cpath");

		// change back to the unit dir for the rest of the tests
		changeDirectory(unitDir);
	}

	static function getCsDependencies() {
		switch (systemName) {
			case "Linux":
				if (commandSucceed("mono", ["--version"]))
					infoMsg('mono has already been installed.');
				else
					requireAptPackages(["mono-devel", "mono-mcs"]);
				runCommand("mono", ["--version"]);
			case "Mac":
				if (commandSucceed("mono", ["--version"]))
					infoMsg('mono has already been installed.');
				else
					runCommand("brew", ["install", "mono"], true);
				runCommand("mono", ["--version"]);
			case "Windows":
				switch (ci) {
					case AppVeyor:
						addToPATH("C:\\Program Files (x86)\\Mono\\bin");
						runCommand("mono", ["--version"]);
					case _:
						//pass
				}
		}

		haxelibInstallGit("HaxeFoundation", "hxcs", true);
	}

	static var gotOpenFLDependencies = false;
	static function getOpenFLDependencies() {
		if (gotOpenFLDependencies) return;

		getCppDependencies();

		haxelibInstallGit("HaxeFoundation", "format");
		haxelibInstallGit("haxenme", "nme");
		haxelibInstallGit("haxenme", "nme-dev");
		haxelibInstallGit("openfl", "svg");
		haxelibInstallGit("openfl", "lime");
		haxelibInstallGit("openfl", "lime-tools");
		haxelibInstallGit("openfl", "openfl-native");
		haxelibInstallGit("openfl", "openfl-html5");
		haxelibInstallGit("openfl", "openfl");

		switch (systemName) {
			case "Linux":
				haxelibRun(["openfl", "rebuild", "linux"]);
			case "Mac":
				haxelibRun(["openfl", "rebuild", "mac"]);
		}
		haxelibRun(["openfl", "rebuild", "tools"]);

		gotOpenFLDependencies = true;
	}

	/**
		Install python and return the names of the installed pythons.
	*/
	static function getPythonDependencies():Array<String> {
		switch (systemName) {
			case "Linux":
				if (commandSucceed("python3", ["-V"]))
					infoMsg('python3 has already been installed.');
				else
					requireAptPackages(["python3"]);
				runCommand("python3", ["-V"]);

				var pypy = "pypy3";
				if (commandSucceed(pypy, ["-V"])) {
					infoMsg('pypy3 has already been installed.');
				} else {
					var pypyVersion = "pypy3-2.4.0-linux64";
					runCommand("wget", ['https://bitbucket.org/pypy/pypy/downloads/${pypyVersion}.tar.bz2'], true);
					runCommand("tar", ["-xf", '${pypyVersion}.tar.bz2']);
					pypy = FileSystem.fullPath('${pypyVersion}/bin/pypy3');
				}
				runCommand(pypy, ["-V"]);

				return ["python3", pypy];
			case "Mac":
				if (commandSucceed("python3", ["-V"]))
					infoMsg('python3 has already been installed.');
				else
					runCommand("brew", ["install", "python3"], true);
				runCommand("python3", ["-V"]);

				if (commandSucceed("pypy3", ["-V"]))
					infoMsg('pypy3 has already been installed.');
				else
					runCommand("brew", ["install", "pypy3"], true);
				runCommand("pypy3", ["-V"]);

				return ["python3", "pypy3"];
			case "Windows":
				if (commandSucceed("python3", ["-V"]))
					infoMsg('python3 has already been installed.');
				else
					throw "please install python 3.x and make it available as python3 in PATH";
				runCommand("python3", ["-V"]);
				return ["python3"];
		}

		return [];
	}

	static var ci(default, never):Null<Ci> =
		if (Sys.getEnv("TRAVIS") == "true")
			TravisCI;
		else if (Sys.getEnv("APPVEYOR") == "True")
			AppVeyor;
		else
			null;
	static var systemName(default, never) = Sys.systemName();
	static var cwd(default, never) = Sys.getCwd();
	static var repoDir(default, never) = FileSystem.fullPath("..") + "/";
	static var unitDir(default, never) = cwd + "unit/";
	static var sysDir(default, never) = cwd + "sys/";
	static var optDir(default, never) = cwd + "optimization/";
	static var miscDir(default, never) = cwd + "misc/";
	static var displayDir(default, never) = cwd + "display/";
	static var gitInfo(get, null):{repo:String, branch:String, commit:String, timestamp:Float, date:String};
	static var success(default, null) = true;
	static function get_gitInfo() return if (gitInfo != null) gitInfo else gitInfo = {
		repo: switch (ci) {
			case TravisCI:
				Sys.getEnv("TRAVIS_REPO_SLUG");
			case AppVeyor:
				Sys.getEnv("APPVEYOR_PROJECT_SLUG");
			case _:
				commandResult("git", ["config", "--get", "remote.origin.url"]).stdout.trim();
		},
		branch: switch (ci) {
			case TravisCI:
				Sys.getEnv("TRAVIS_BRANCH");
			case AppVeyor:
				Sys.getEnv("APPVEYOR_REPO_BRANCH");
			case _:
				commandResult("git", ["rev-parse", "--abbrev-ref", "HEAD"]).stdout.trim();
		},
		commit: commandResult("git", ["rev-parse", "HEAD"]).stdout.trim(),
		timestamp: Std.parseFloat(commandResult("git", ["show", "-s", "--format=%ct", "HEAD"]).stdout),
		date: {
			var gitTime = commandResult("git", ["show", "-s", "--format=%ct", "HEAD"]).stdout;
			var tzd = {
				var z = Date.fromTime(0);
				z.getHours() * 60 * 60 * 1000 + z.getMinutes() * 60 * 1000;
			}
			// make time in the UTC time zone
			var time = Date.fromTime(Std.parseFloat(gitTime) * 1000 - tzd);
			DateTools.format(time, "%FT%TZ");
		}
	}
	static var haxeVer(default, never) = {
		var haxe_ver = haxe.macro.Compiler.getDefine("haxe_ver");
		switch (haxe_ver.split(".")) {
			case [major]:
				major;
			case [major, minor] if (minor.length == 1):
				'${major}.${minor}';
			case [major, minor] if (minor.length > 1):
				var minor = minor.charAt(0);
				var patch = Std.parseInt(minor.substr(1));
				'${major}.${minor}.${patch}';
			case _:
				throw haxe_ver;
		}
	}
	static var haxeVerFull(default, never) = {
		var ver = haxeVer.split(".");
		while (ver.length < 3) {
			ver.push("0");
		}
		ver.join(".");
	}

	static function deploy():Void {
		if (
			Sys.getEnv("DEPLOY") != null
		) {
			changeDirectory(repoDir);

			// generate doc
			runCommand("make", ["-s", "install_dox"]);
			runCommand("make", ["-s", "package_doc"]);

			// deployBintray();
			deployApiDoc();
			deployPPA();
		}
	}

	static function deployBintray():Void {
		if (
			Sys.getEnv("BINTRAY") != null &&
			Sys.getEnv("BINTRAY_USERNAME") != null &&
			Sys.getEnv("BINTRAY_API_KEY") != null
		) {
			// generate bintray config
			var tpl = new Template(File.getContent("extra/bintray.tpl.json"));
			var compatDate = ~/[^0-9]/g.replace(gitInfo.date, "");
			var json = tpl.execute({
				packageSubject: {
					var sub = Sys.getEnv("BINTRAY_SUBJECT");
					sub != null ? sub : Sys.getEnv("BINTRAY_USERNAME");
				},
				os: systemName.toLowerCase(),
				versionName: '${haxeVer}+${compatDate}.${gitInfo.commit.substr(0,7)}',
				versionDesc: "Automated CI build.",
				gitRepo: gitInfo.repo,
				gitBranch: gitInfo.branch,
				gitCommit: gitInfo.commit,
				gitDate: gitInfo.date,
			});
			var path = "extra/bintray.json";
			File.saveContent("extra/bintray.json", json);
			infoMsg("saved " + FileSystem.absolutePath(path) + " with content:");
			Sys.println(json);
		}
	}

	/**
		Deploy doc to api.haxe.org.
	*/
	static function deployApiDoc():Void {
		if (
			gitInfo.branch == "development" &&
			Sys.getEnv("DEPLOY") != null &&
			Sys.getEnv("deploy_key_decrypt") != null
		) {
			// setup deploy_key
			runCommand("openssl aes-256-cbc -k \"$deploy_key_decrypt\" -in extra/deploy_key.enc -out extra/deploy_key -d");
			runCommand("chmod 600 extra/deploy_key");
			runCommand("ssh-add extra/deploy_key");

			runCommand("make", ["-s", "deploy_doc"]);
		}
	}

	/**
		Deploy source package to ppa:haxe/snapshots.
	*/
	static function deployPPA():Void {
		if (
			gitInfo.branch == "development" &&
			Sys.getEnv("DEPLOY") != null &&
			Sys.getEnv("haxeci_decrypt") != null
		) {
			// setup haxeci_ssh
			runCommand("openssl aes-256-cbc -k \"$haxeci_decrypt\" -in extra/haxeci_ssh.enc -out extra/haxeci_ssh -d");
			runCommand("chmod 600 extra/haxeci_ssh");
			runCommand("ssh-add extra/haxeci_ssh");
			// setup haxeci_sec.gpg
			runCommand("openssl aes-256-cbc -k \"$haxeci_decrypt\" -in extra/haxeci_sec.gpg.enc -out extra/haxeci_sec.gpg -d");
			runCommand("gpg --allow-secret-key-import --import extra/haxeci_sec.gpg");
			runCommand("sudo apt-get install devscripts git-buildpackage ubuntu-dev-tools dh-make -y");
			var compatDate = ~/[^0-9]/g.replace(gitInfo.date, "");
			var SNAPSHOT_VERSION = '${haxeVerFull}+1SNAPSHOT${compatDate}+${gitInfo.commit.substr(0,7)}';
			runCommand('cp out/haxe*_src.tar.gz "../haxe_${SNAPSHOT_VERSION}.orig.tar.gz"');
			changeDirectory("..");
			runCommand("git clone https://github.com/HaxeFoundation/haxe-debian.git");
			changeDirectory("haxe-debian");
			runCommand("git checkout upstream");
			runCommand("git checkout next");
			runCommand('gbp import-orig "../haxe_${SNAPSHOT_VERSION}.orig.tar.gz" -u "${SNAPSHOT_VERSION}" --debian-branch=next');
			runCommand('dch -v "1:${SNAPSHOT_VERSION}-1" --urgency low "snapshot build"');
			runCommand("debuild -S -sa");
			runCommand("backportpackage -d xenial  --upload ${PPA} --yes ../haxe_*.dsc");
			runCommand("backportpackage -d wily    --upload ${PPA} --yes ../haxe_*.dsc");
			runCommand("backportpackage -d vivid   --upload ${PPA} --yes ../haxe_*.dsc");
			runCommand("backportpackage -d trusty  --upload ${PPA} --yes ../haxe_*.dsc");
			runCommand("git checkout debian/changelog");
			runCommand("git config --global user.name \"${DEBFULLNAME}\"");
			runCommand("git config --global user.email \"${DEBEMAIL}\"");
			runCommand("git merge -X ours --no-edit origin/backport-precise");
			runCommand('dch -v "1:${SNAPSHOT_VERSION}-1" --urgency low "snapshot build"');
			runCommand("debuild -S -sa");
			runCommand("backportpackage -d precise --upload ${PPA} --yes ../haxe_*.dsc");
		}
	}

	static function saveOutput():Void {
		if (Sys.getEnv("HAXECI_GH_TOKEN") == null) {
			infoMsg("Missing HAXECI_GH_TOKEN. Will not save output.");
			return;
		}

		changeDirectory(repoDir);
		var haxe_output = Path.join([repoDir, "haxe-output"]);
		var gitInfo = gitInfo;
		var TEST = Sys.getEnv("TEST");
		var TRAVIS_OS_NAME = Sys.getEnv("TRAVIS_OS_NAME");
		var haxe_output_branch = '${gitInfo.branch}_travisci_${TRAVIS_OS_NAME}_${TEST}';
		var haxe_output_repo = "github.com/HaxeFoundation/haxe-output.git";

		function haxeCommitTime(sha:String):Float {
			var cwd = Sys.getCwd();
			Sys.setCwd(repoDir);
			var time = Std.parseFloat(commandResult("git", ["show", "-s", "--pretty=%ct", sha]).stdout);
			Sys.setCwd(cwd);
			return time;
		}

		function save():Void {
			// prepare haxe-output repo
			runCommand("git", ["clone", 'https://${Sys.getEnv("HAXECI_GH_TOKEN")}@${haxe_output_repo}', haxe_output]);
			changeDirectory(haxe_output);
			runCommand("git", ["config", "user.email", "haxe-ci@onthewings.net"]);
			runCommand("git", ["config", "user.name", "Haxe CI Bot"]);

			// check to see whether the haxe repo branch has been created in haxe-output
			var firstOutputBranch = switch (Sys.command("git", ["ls-remote", "--exit-code", "origin", haxe_output_branch])) {
				case 0: false;
				case 2: true;
				case exitcode: throw "unknown exit code: " + exitcode;
			}

			// switch to the branch
			if (firstOutputBranch) {
				runCommand("git", ["checkout", "-B", haxe_output_branch]);
			} else {
				runCommand("git", ["checkout", "-B", haxe_output_branch, "--track", "origin/" + haxe_output_branch]);
			}

			// check to see whether this will be the first push of this haxe repo commit
			var firstOutputCommit = firstOutputBranch || {
				var lastLog = commandResult("git", ["show", "-s", "--pretty=format:%B", "HEAD"]).stdout;
				var commitRe = ~/[a-f0-9]{40}/;
				if (!commitRe.match(lastLog)) {
					infoMsg("No commit sha found in log: " + lastLog);
					true;
				} else {
					var lastCommit = commitRe.matched(0);

					// check to see whether this commit is newer than the last pushed one
					// in case e.g. the current build is a rebuild of an old commit
					if (haxeCommitTime(lastCommit) > gitInfo.timestamp) {
						throw "There exists output with a later commit in the haxe-output repo.";
					}

					lastCommit != gitInfo.commit;
				}
			};

			// Remove all the old output first.
			// It is because there may be files no longer emitted by the current build.
			if (firstOutputCommit) {
				runCommand("rm", ["-rf", "tests"]);
			}

			// get the outputs by listing the files/folders ignored by git
			changeDirectory(cwd);
			var stdout = {
				var proc = new Process("git", ["status", "--ignored", "--porcelain", cwd]);
				var out = proc.stdout.readAll().toString();
				proc.close();
				out;
			};
			var outputs = stdout.split("\n")
				.filter(function(s) return s.startsWith("!! "))
				.map(function(s) return s.substr("!! ".length));

			// copy all the outputs to haxe-output
			FileSystem.createDirectory(haxe_output);
			for (item in outputs) {
				var orig = Path.join([repoDir, item]);
				var dest = Path.join([haxe_output, item]);
				if (FileSystem.isDirectory(orig)) {
					FileSystem.createDirectory(dest);
				} else {
					FileSystem.createDirectory(Path.directory(dest));
				}
				runCommand("cp", ["-rf", orig, dest]);
			}
			changeDirectory(haxe_output);
			runCommand("git", ["add", "--all", haxe_output]);
			var commitMsg = [
				'-m', '${Sys.getEnv("TRAVIS_JOB_NUMBER")} ${TEST} https://github.com/HaxeFoundation/haxe/commit/${gitInfo.commit}',
				'-m', 'https://travis-ci.org/HaxeFoundation/haxe/jobs/${Sys.getEnv("TRAVIS_JOB_ID")}',
			];
			runCommand("git", ["commit", "-q", "--allow-empty"].concat(commitMsg));
		}

		// try save() for 5 times because the push may fail when the another build push at the same time
		var pushError = false;
		for (i in 0...5) {
			try {
				save();
			} catch (e:Dynamic) {
				infoMsg(e);
				pushError = false;
				break;
			}
			var pushResult = commandResult("git", ["push", "origin", haxe_output_branch]);
			if (pushResult.exitCode == 0) {
				successMsg("push to haxe-output succeeded");
				pushError = false;
				break;
			} else {
				failMsg("push to haxe-output failed");
				failMsg(pushResult.stderr);
				pushError = true;

				runCommand("rm", ["-rf", haxe_output]);

				Sys.sleep(Std.random(10));
			}
		}
		if (pushError) {
			fail();
		}
	}

	static function main():Void {
		Sys.putEnv("OCAMLRUNPARAM", "b");

		var tests:Array<TEST> = switch (Sys.getEnv("TEST")) {
			case null:
				[Macro];
			case env:
				[for (v in env.split(",")) v.trim().toLowerCase()];
		}
		Sys.println('Going to test: $tests');

		for (test in tests) {
			switch (ci) {
				case TravisCI:
					Sys.println('travis_fold:start:test-${test}');
				case _:
					//pass
			}
			
			infoMsg('test $test');
			var success = true;
			try {
				changeDirectory(unitDir);


				var args = switch (ci) {
					case TravisCI:
						["-D","travis"];
					case AppVeyor:
						["-D","appveyor"];
					case _:
						[];
				}
				switch (test) {
					case Macro:
						runCommand("haxe", ["compile-macro.hxml"].concat(args));

						changeDirectory(displayDir);
						runCommand("haxe", ["build.hxml"]);

						changeDirectory(miscDir);
						getCsDependencies();
						getPythonDependencies();
						runCommand("haxe", ["compile.hxml"]);

						changeDirectory(sysDir);
						runCommand("haxe", ["compile-macro.hxml"]);
						runCommand("haxe", ["compile-each.hxml", "--run", "Main"]);
					case Neko:
						runCommand("haxe", ["compile-neko.hxml", "-D", "dump", "-D", "dump_ignore_var_ids"].concat(args));
						runCommand("neko", ["bin/unit.n"]);

						changeDirectory(sysDir);
						runCommand("haxe", ["compile-neko.hxml"]);
						runCommand("neko", ["bin/neko/sys.n"]);
					case Php:
						getPhpDependencies();
						runCommand("haxe", ["compile-php.hxml"].concat(args));
						runCommand("php", ["bin/php/index.php"]);

						changeDirectory(sysDir);
						runCommand("haxe", ["compile-php.hxml"]);
						runCommand("php", ["bin/php/Main/index.php"]);
					case Python:
						var pys = getPythonDependencies();

						runCommand("haxe", ["compile-python.hxml"].concat(args));
						for (py in pys) {
							runCommand(py, ["bin/unit.py"]);
						}

						changeDirectory(sysDir);
						runCommand("haxe", ["compile-python.hxml"]);
						for (py in pys) {
							runCommand(py, ["bin/python/sys.py"]);
						}

						changeDirectory(miscDir + "pythonImport");
						runCommand("haxe", ["compile.hxml"]);
						for (py in pys) {
							runCommand(py, ["test.py"]);
						}
					case Lua:
						getLuaDependencies();
						runCommand("haxe", ["compile-lua.hxml"].concat(args));
						runCommand("lua", ["bin/unit.lua"]);
					case Cpp:
						getCppDependencies();
						runCommand("haxe", ["compile-cpp.hxml", "-D", "HXCPP_M32"].concat(args));
						runCpp("bin/cpp/Test-debug", []);

						switch (ci) {
							case AppVeyor:
								//save time...
							case _:
								runCommand("rm", ["-rf", "cpp"]);
								runCommand("haxe", ["compile-cpp.hxml", "-D", "HXCPP_M64"].concat(args));
								runCpp("bin/cpp/Test-debug", []);
						}

						changeDirectory(sysDir);
						runCommand("haxe", ["compile-cpp.hxml"]);
						runCpp("bin/cpp/Main-debug", []);

						if (Sys.systemName() == "Mac")
						{
							changeDirectory(miscDir + "cppObjc");
							runCommand("haxe", ["build.hxml"]);
							runCpp("bin/TestObjc-debug");
						}
					case Js:
						getJSDependencies();

						var jsOutputs = [
							for (es3 in       [[], ["-D", "js-es=3"]])
							for (unflatten in [[], ["-D", "js-unflatten"]])
							for (classic in   [[], ["-D", "js-classic"]])
							{
								var extras = args.concat(es3).concat(unflatten).concat(classic);

								runCommand("haxe", ["compile-js.hxml"].concat(extras));

								var output = if (extras.length > 0) {
									"bin/js/" + extras.join("") + "/unit.js";
								} else {
									"bin/js/default/unit.js";
								}
								var outputDir = Path.directory(output);
								if (!FileSystem.exists(outputDir)) {
									FileSystem.createDirectory(outputDir);
								}
								FileSystem.rename("bin/unit.js", output);
								FileSystem.rename("bin/unit.js.map", output + ".map");
								runCommand("node", ["-e", "var unit = require('./" + output + "').unit; unit.Test.main(); process.exit(unit.Test.success ? 0 : 1);"]);
								output;
							}
						];

						var env = Sys.environment();
						if (
							env.exists("SAUCE") &&
							env.exists("SAUCE_USERNAME") &&
							env.exists("SAUCE_ACCESS_KEY")
						) {
							// sauce-connect should have been started

							// var scVersion = "sc-4.3-linux";
							// runCommand("wget", ['https://saucelabs.com/downloads/${scVersion}.tar.gz'], true);
							// runCommand("tar", ["-xf", '${scVersion}.tar.gz']);

							// //start sauce-connect
							// var scReadyFile = "sauce-connect-ready-" + Std.random(100);
							// var sc = new Process('${scVersion}/bin/sc', [
							// 	"-i", Sys.getEnv("TRAVIS_JOB_NUMBER"),
							// 	"-f", scReadyFile
							// ]);
							// while(!FileSystem.exists(scReadyFile)) {
							// 	Sys.sleep(0.5);
							// }

							runCommand("npm", ["install", "wd", "q"], true);
							haxelibInstall("hxnodejs");
							runCommand("haxe", ["compile-saucelabs-runner.hxml"]);
							var server = new Process("nekotools", ["server"]);
							runCommand("node", ["bin/RunSauceLabs.js"].concat([for (js in jsOutputs) "unit-js.html?js=" + js.urlEncode()]));

							server.close();
							// sc.close();
						}

						infoMsg("Test optimization:");
						changeDirectory(optDir);
						runCommand("haxe", ["run.hxml"]);
					case Java:
						getJavaDependencies();
						runCommand("haxe", ["compile-java.hxml"].concat(args));
						runCommand("java", ["-jar", "bin/java/Test-Debug.jar"]);

						runCommand("haxe", ["compile-java.hxml","-dce","no"].concat(args));
						runCommand("java", ["-jar", "bin/java/Test-Debug.jar"]);

						changeDirectory(sysDir);
						runCommand("haxe", ["compile-java.hxml"]);
						runCommand("java", ["-jar", "bin/java/Main-Debug.jar"]);

						infoMsg("Testing java-lib extras");
						changeDirectory('$unitDir/bin');
						runCommand("git", ["clone", "https://github.com/waneck/java-lib-tests.git", "--depth", "1"], true);
						for (dir in FileSystem.readDirectory('java-lib-tests'))
						{
							var path = 'java-lib-tests/$dir';
							if (FileSystem.isDirectory(path)) for (file in FileSystem.readDirectory(path))
							{
								if (file.endsWith('.hxml'))
								{
									runCommand("haxe", ["--cwd",'java-lib-tests/$dir',file]);
								}
							}
						}

					case Cs:
						getCsDependencies();

						var compl = switch [ci, systemName] {
							case [TravisCI, "Linux"]:
								"-travis";
							case _:
								"";
						};

						runCommand("haxe", ['compile-cs$compl.hxml']);
						runCs("bin/cs/bin/Test-Debug.exe");

						runCommand("haxe", ['compile-cs$compl.hxml','-D','fast_cast']);
						runCs("bin/cs/bin/Test-Debug.exe");

						runCommand("haxe", ['compile-cs$compl.hxml','-dce','no','-D','fast_cast']);
						runCs("bin/cs/bin/Test-Debug.exe");

						runCommand("haxe", ['compile-cs-unsafe$compl.hxml','-D','fast_cast']);
						runCs("bin/cs_unsafe/bin/Test-Debug.exe");

						runCommand("haxe", ['compile-cs$compl.hxml',"-D","erase_generics",'-D','fast_cast']);
						runCs("bin/cs/bin/Test-Debug.exe");

						runCommand("haxe", ['compile-cs-unsafe$compl.hxml',"-D","erase_generics",'-D','fast_cast']);
						runCs("bin/cs_unsafe/bin/Test-Debug.exe");

						runCommand("haxe", ['compile-cs$compl.hxml',"-D","no_root",'-D','fast_cast']);
						runCs("bin/cs/bin/Test-Debug.exe");

						runCommand("haxe", ['compile-cs-unsafe$compl.hxml',"-D","no_root","-D","erase_generics",'-D','fast_cast']);
						runCs("bin/cs_unsafe/bin/Test-Debug.exe");

						changeDirectory(sysDir);
						runCommand("haxe", ["compile-cs.hxml",'-D','fast_cast']);
						runCs("bin/cs/bin/Main-Debug.exe", []);

						changeDirectory(miscDir + "csTwoLibs");
						for (i in 1...5)
						{
							runCommand("haxe", ['compile-$i.hxml','-D','fast_cast']);
							runCs("bin/main/bin/Main.exe");
						}

					case Flash9:
						setupFlashPlayerDebugger();
						runCommand("haxe", ["compile-flash9.hxml", "-D", "fdb", "-D", "dump", "-D", "dump_ignore_var_ids"].concat(args));
						var success = runFlash("bin/unit9.swf");
						if (!success)
							fail();
					case As3:
						setupFlashPlayerDebugger();

						//setup flex sdk
						if (commandSucceed("mxmlc", ["--version"])) {
							infoMsg('mxmlc has already been installed.');
						} else {
							var flexVersion = "4.14.1";
							runCommand("wget", ['http://archive.apache.org/dist/flex/${flexVersion}/binaries/apache-flex-sdk-${flexVersion}-bin.tar.gz'], true);
							runCommand("tar", ["-xf", 'apache-flex-sdk-${flexVersion}-bin.tar.gz', "-C", Sys.getEnv("HOME")]);
							var flexsdkPath = Sys.getEnv("HOME") + '/apache-flex-sdk-${flexVersion}-bin';
							addToPATH(flexsdkPath + "/bin");
							var playerglobalswcFolder = flexsdkPath + "/player";
							FileSystem.createDirectory(playerglobalswcFolder + "/11.1");
							runCommand("wget", ["-nv", "http://download.macromedia.com/get/flashplayer/updaters/11/playerglobal11_1.swc", "-O", playerglobalswcFolder + "/11.1/playerglobal.swc"], true);
							File.saveContent(flexsdkPath + "/env.properties", 'env.PLAYERGLOBAL_HOME=$playerglobalswcFolder');
							runCommand("mxmlc", ["--version"]);
						}

						runCommand("haxe", ["compile-as3.hxml", "-D", "fdb"].concat(args));
						var success = runFlash("bin/unit9_as3.swf");
						if (!success)
							fail();
					case Hl:
						runCommand("haxe", ["compile-hl.hxml"]);
					case ThirdParty:
						getPhpDependencies();
						getJavaDependencies();
						getJSDependencies();
						getCsDependencies();
						getPythonDependencies();
						getCppDependencies();
						//getOpenFLDependencies();

						//testPolygonalDs();
						// if (systemName == "Linux") testFlambe(); //#3439
						testHxTemplo();
						testMUnit();
						testHaxeQuake();
						//testOpenflSamples();
						//testFlixelDemos();
					case t:
						throw "unknown target: " + t;
				}
			} catch(f:Failure) {
				success = false;
			}
			
			switch (ci) {
				case TravisCI:
					Sys.println('travis_fold:end:test-${test}');
				case _:
					//pass
			}
			
			if (success) {
				successMsg('test ${test} succeeded');
			} else {
				failMsg('test ${test} failed');
			}
		}

		if (
			(switch [ci, systemName] {
				case [TravisCI, "Linux"]: true;
				case _: false;
			})
			&&
			Lambda.exists(tests, function(t) return switch (t) {
				case Js | Cpp | Cs | Java | Php | Python | Neko | Flash9 | As3: true;
				case _: false;
			})
		) {
			saveOutput();
		}

		if (success) {
			deploy();
		} else {
			Sys.exit(1);
		}
	}

	static function testHxTemplo() {
		infoMsg("Test hx-templo:");

		changeDirectory(unitDir);

		haxelibInstallGit("Simn", "hxparse", "development", "src");
		haxelibInstallGit("Simn", "hxtemplo");

		var buildArgs = [
			"-cp", "src",
			"-cp", "test",
			"-main", "Test",
			"-lib", "hxparse",
			"-dce", "full"
		];

		changeDirectory(getHaxelibPath("hxtemplo") + "..");
		runCommand("haxe", ["build.hxml"]);
	}

	static function testPolygonalDs() {
		infoMsg("Test polygonal-ds:");

		changeDirectory(unitDir);
		haxelibInstallGit("Simn", "ds", "python-support", null, false, "polygonal-ds");
		haxelibInstallGit("polygonal", "core", "master", "src", false, "polygonal-core");
		haxelibInstallGit("polygonal", "printf", "master", "src", false, "polygonal-printf");
		changeDirectory(getHaxelibPath("polygonal-ds"));
		runCommand("haxe", ["build.hxml"]);
		runCommand("python3", ["unit.py"]);
		runCommand("node", ["unit.js"]);
	}

	static function testMUnit() {
		infoMsg("Test MUnit:");

		changeDirectory(unitDir);

		haxelibInstallGit("massiveinteractive", "mconsole", "master", "src");
		haxelibInstallGit("massiveinteractive", "MassiveCover", "master", "src", false, "mcover");
		haxelibInstallGit("massiveinteractive", "MassiveLib", "master", "src", false, "mlib");
		haxelibInstallGit("massiveinteractive", "MassiveUnit", "2.1.2", "src", false, "munit");
		changeDirectory(Path.join([getHaxelibPath("munit"), "..", "tool"]));
		runCommand("haxe", ["build.hxml"]);
		haxelibRun(["munit", "test", "-result-exit-code", "-neko"], true);
		changeDirectory("../");
		haxelibRun(["munit", "test", "-result-exit-code", "-neko"], true);
	}

	static function testHaxeQuake() {
		infoMsg("Test HaxeQuake:");

		changeDirectory(unitDir);
		runCommand("git", ["clone", "https://github.com/nadako/HaxeQuake"]);
		changeDirectory("HaxeQuake/Client");
		runCommand("haxe", ["build.hxml"]);
	}

	static function testFlambe() {
		infoMsg("Test Flambe:");

		changeDirectory(unitDir);
		runCommand("git", ["clone", "https://github.com/aduros/flambe"]);
		runCommand("sh", ["flambe/bin/run-travis"]);
	}

	//static function testOpenflSamples() {
		//infoMsg("Test OpenFL Samples:");
//
		//changeDirectory(unitDir);
//
		//haxelibInstallGit("jgranick", "actuate");
		//haxelibInstallGit("jgranick", "box2d");
		//haxelibInstallGit("jgranick", "layout");
		//haxelibInstallGit("openfl", "swf");
		//haxelibInstallGit("openfl", "openfl-samples");
//
		//var path = getHaxelibPath("openfl-samples");
		//var old = Sys.getEnv("pwd");
		//Sys.putEnv("pwd", path);
		//parseTravisFile(haxe.io.Path.join([path, ".travis.yml"]), true);
		//if (old != null) {
			//Sys.putEnv("pwd", old);
		//}
	//}

	static function testFlixelDemos() {
		infoMsg("Test Flixel Demos:");

		changeDirectory(unitDir);
		getOpenFLDependencies();

		haxelibInstall("systools");
		haxelibInstall("spinehx");
		haxelibInstall("nape");
		haxelibInstall("task");

		haxelibInstallGit("larsiusprime", "firetongue");
		haxelibInstallGit("YellowAfterLife", "openfl-bitfive");

		haxelibInstallGit("HaxeFlixel", "flixel");
		haxelibInstallGit("HaxeFlixel", "flixel-addons");
		haxelibInstallGit("HaxeFlixel", "flixel-ui");
		haxelibInstallGit("HaxeFlixel", "flixel-demos");
		haxelibInstallGit("HaxeFlixel", "flixel-tools");

		haxelibRun(["flixel-tools", "testdemos", "-flash"]);
		haxelibRun(["flixel-tools", "testdemos", "-neko"]);
		haxelibRun(["flixel-tools", "testdemos", "-html5"]);
	}
}

