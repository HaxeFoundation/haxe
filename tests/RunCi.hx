using StringTools;

import sys.*;
import sys.io.*;
import haxe.*;
import haxe.io.*;

/**
	List of "TEST" defined in the "matrix" section of ".travis.yml".
*/
@:enum abstract TEST(String) from String {
	var Macro = "macro";
	var Neko = "neko";
	var Js = "js";
	var Lua = "lua";
	var Php = "php";
	var Php7 = "php7";
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

	static var S3_HXBUILDS_ADDR(default, null) = 's3://hxbuilds/builds/haxe';

	/**
		Run a command using `Sys.command()`.
		If the command exits with non-zero code, exit the whole script with the same code.

		If `useRetry` is `true`, the command will be re-run if it exits with non-zero code (3 trials).
		It is useful for running network-dependent commands.
	*/
	static function runCommand(cmd:String, ?args:Array<String>, useRetry:Bool = false, allowFailure:Bool = false):Void {
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

		if (!allowFailure)
			fail();
	}

	static function isAptPackageInstalled(aptPackage:String):Bool {
		return commandSucceed("dpkg-query", ["-W", "-f='${Status}'", aptPackage]);
	}

	static function requireAptPackages(packages:Array<String>):Void {
		var notYetInstalled = [for (p in packages) if (!isAptPackageInstalled(p)) p];
		if (notYetInstalled.length > 0) {
			var aptCacheDir = Sys.getEnv("APT_CACHE_DIR");
			var baseCommand = if (aptCacheDir != null) {
				["apt-get", "-o", 'dir::cache::archives=${aptCacheDir}', "install", "-y"];
			} else {
				["apt-get", "install", "-y"];
			};
			runCommand("sudo", baseCommand.concat(notYetInstalled), true);
		}

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
					"libglib2.0", "libfreetype6"
				]);
				runCommand("wget", ["-nv", "http://fpdownload.macromedia.com/pub/flashplayer/updaters/25/flash_player_sa_linux_debug.x86_64.tar.gz"], true);
				runCommand("tar", ["-xf", "flash_player_sa_linux_debug.x86_64.tar.gz", "-C", Sys.getEnv("HOME")]);
				File.saveContent(mmcfgPath, "ErrorReportingEnable=1\nTraceOutputFileEnable=1");
				runCommand(Sys.getEnv("HOME") + "/flashplayerdebugger", ["-v"]);
			case "Mac":
				runCommand("brew", ["tap", "caskroom/versions"]);
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
				var phpCmd = commandResult("php", ["-v"]);
				var phpVerReg = ~/PHP ([0-9]+\.[0-9]+)/i;
				var phpVer = if (phpVerReg.match(phpCmd.stdout))
					Std.parseFloat(phpVerReg.matched(1));
				else
					null;
				if (phpCmd.exitCode == 0 && phpVer != null && phpVer >= 5.5) {
					infoMsg('php has already been installed.');
				} else {
					requireAptPackages(["php5-cli"]);
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
			runCommand("haxe", ["-D", "source-header=''", "compile.hxml"]);
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

	static function getLuaDependencies(){
		switch (systemName){
			case "Linux":
				requireAptPackages(["libpcre3-dev"]);
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

	static function installLuaVersionDependencies(lv:String){
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
	static var serverDir(default, never) = cwd + "server/";
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
			DateTools.format(time, "%Y-%m-%dT%H:%M:%SZ");
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
				var patch = Std.parseInt(minor.substr(1));
				var minor = minor.charAt(0);
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

	static function isDeployNightlies () {
		return Sys.getEnv("DEPLOY_NIGHTLIES") != null;
	}
	static function deploy():Void {

		var doDocs = isDeployApiDocsRequired();
		var doNightlies = isDeployNightlies(),
				doInstaller = doNightlies && shouldDeployInstaller();

		if (doDocs || doNightlies) {
			changeDirectory(repoDir);
			if (doDocs) {
				if (systemName != 'Windows') {
					// generate doc
					runCommand("make", ["-s", "install_dox"]);
					runCommand("make", ["-s", "package_doc"]);
					// deployBintray();
					deployApiDoc();
					// disable deployment to ppa:haxe/snapshots for now
					// because there is no debian sedlex package...
					// deployPPA();
				}
			}
			if (doNightlies) {
				if (doInstaller && !doDocs && systemName != 'Windows') {
					// generate doc
					runCommand("make", ["-s", "install_dox"]);
					runCommand("make", ["-s", "package_doc"]);
				}
				deployNightlies(doInstaller);
			}
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

	static function shouldDeployInstaller() {
		if (systemName == 'Linux') {
			return false;
		}
		if (gitInfo.branch == 'nightly-travis') {
			return true;
		}
		var rev = Sys.getEnv('ADD_REVISION');
		return rev != null && rev != "0";
	}

	static function isDeployApiDocsRequired () {
		return gitInfo.branch == "development" &&
			Sys.getEnv("DEPLOY_API_DOCS") != null &&
			Sys.getEnv("deploy_key_decrypt") != null;
	}

	/**
		Deploy doc to api.haxe.org.
	*/

	static function deployApiDoc():Void {
		// setup deploy_key
		runCommand("openssl aes-256-cbc -k \"$deploy_key_decrypt\" -in extra/deploy_key.enc -out extra/deploy_key -d");
		runCommand("chmod 600 extra/deploy_key");
		runCommand("ssh-add extra/deploy_key");

		runCommand("make", ["-s", "deploy_doc"]);
	}

	/**
		Deploy source package to hxbuilds s3
	*/
	static function deployNightlies(doInstaller:Bool):Void {
		var gitTime = commandResult("git", ["show", "-s", "--format=%ct", "HEAD"]).stdout;
		var tzd = {
			var z = Date.fromTime(0);
			z.getHours() * 60 * 60 * 1000 + z.getMinutes() * 60 * 1000;
		};
		var time = Date.fromTime(Std.parseFloat(gitTime) * 1000 - tzd);
		if (
			(gitInfo.branch == "development" ||
			gitInfo.branch == "master" ||
			gitInfo.branch == "nightly-travis") &&
			Sys.getEnv("HXBUILDS_AWS_ACCESS_KEY_ID") != null &&
			Sys.getEnv("HXBUILDS_AWS_SECRET_ACCESS_KEY") != null &&
			Sys.getEnv("TRAVIS_PULL_REQUEST") != "true"
		) {
			if (ci == TravisCI) {
				runCommand("make", ["-s", "package_unix"]);
				if (doInstaller) {
					getLatestNeko();
					runCommand("make", ["-s", 'package_installer_mac']);
				}
				if (systemName == 'Linux') {
					// source
					for (file in sys.FileSystem.readDirectory('out')) {
						if (file.startsWith('haxe') && file.endsWith('_src.tar.gz')) {
							submitToS3("source", 'out/$file');
							break;
						}
					}
				}
				for (file in sys.FileSystem.readDirectory('out')) {
					if (file.startsWith('haxe')) {
						if (file.endsWith('_bin.tar.gz')) {
							var name = systemName == "Linux" ? 'linux64' : 'mac';
							submitToS3(name, 'out/$file');
						} else if (file.endsWith('_installer.tar.gz')) {
							submitToS3('mac-installer', 'out/$file');
						}
					}
				}
			} else {
				if (doInstaller) {
					getLatestNeko();
					var cygRoot = Sys.getEnv("CYG_ROOT");
					if (cygRoot != null) {
						runCommand('$cygRoot/bin/bash', ['-lc', "cd \"$OLDPWD\" && make -s -f Makefile.win package_installer_win"]);
					} else {
						runCommand("make", ['-f', 'Makefile.win', "-s", 'package_installer_win']);
					}
				}
				for (file in sys.FileSystem.readDirectory('out')) {
					if (file.startsWith('haxe')) {
						if (file.endsWith('_bin.zip')) {
							submitToS3('windows', 'out/$file');
						} else if (file.endsWith('_installer.zip')) {
							submitToS3('windows-installer', 'out/$file');
						}
					}
				}
			}
		} else {
			trace('Not deploying nightlies');
		}
	}

	static function getLatestNeko() {
		if (!FileSystem.exists('installer')) {
			FileSystem.createDirectory('installer');
		}
		var src = 'http://nekovm.org/media/neko-2.1.0-';
		var suffix = systemName == 'Windows' ? 'win.zip' : 'osx64.tar.gz';
		src += suffix;
		runCommand("wget", [src, '-O', 'installer/neko-$suffix'], true);
	}

	static function createNsiInstaller() {
		if (!FileSystem.exists('installer')) {
			FileSystem.createDirectory('installer');
		}
		getLatestNeko();
	}

	static function fileExtension(file:String) {
		file = haxe.io.Path.withoutDirectory(file);
		var idx = file.indexOf('.');
		if (idx < 0) {
			return '';
		} else {
			return file.substr(idx);
		}
	}

	static function submitToS3(kind:String, sourceFile:String) {
		var date = DateTools.format(Date.now(), '%Y-%m-%d');
		var ext = fileExtension(sourceFile);
		var fileName = 'haxe_${date}_${gitInfo.branch}_${gitInfo.commit.substr(0,7)}${ext}';

		var changeLatest = gitInfo.branch == "development";
		Sys.putEnv('AWS_ACCESS_KEY_ID', Sys.getEnv('HXBUILDS_AWS_ACCESS_KEY_ID'));
		Sys.putEnv('AWS_SECRET_ACCESS_KEY', Sys.getEnv('HXBUILDS_AWS_SECRET_ACCESS_KEY'));
		runCommand('aws s3 cp --region us-east-1 "$sourceFile" "$S3_HXBUILDS_ADDR/$kind/$fileName"');
		if (changeLatest) {
			runCommand('aws s3 cp --region us-east-1 "$sourceFile" "$S3_HXBUILDS_ADDR/$kind/haxe_latest$ext"');
		}
		Indexer.index('$S3_HXBUILDS_ADDR/$kind/');
		runCommand('aws s3 cp --region us-east-1 index.html "$S3_HXBUILDS_ADDR/$kind/index.html"');
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
			// setup deb info
			runCommand("git config --global user.name \"${DEBFULLNAME}\"");
			runCommand("git config --global user.email \"${DEBEMAIL}\"");
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
			runCommand("backportpackage -d yakkety --upload ${PPA} --yes ../haxe_*.dsc");
			runCommand("backportpackage -d xenial  --upload ${PPA} --yes ../haxe_*.dsc");
			runCommand("backportpackage -d vivid   --upload ${PPA} --yes ../haxe_*.dsc");
			runCommand("backportpackage -d trusty  --upload ${PPA} --yes ../haxe_*.dsc");
			runCommand("git checkout debian/changelog");
			runCommand("git merge -X ours --no-edit origin/next-precise");
			runCommand('dch -v "1:${SNAPSHOT_VERSION}-1" --urgency low "snapshot build"');
			runCommand("debuild -S -sa");
			runCommand("backportpackage -d precise --upload ${PPA} --yes ../haxe_*.dsc");
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
					case Php7:
						if (systemName == "Linux") {
								runCommand("phpenv", ["global", "7.0"], false, true);
							runCommand("haxe", ["compile-php7.hxml"].concat(args));
							runCommand("php", ["bin/php7/index.php"]);

							changeDirectory(sysDir);
							runCommand("haxe", ["compile-php7.hxml"]);
							runCommand("php", ["bin/php7/Main/index.php"]);
						}
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
							runCommand("haxe", ["compile-lua.hxml"].concat(args));
							runCommand("lua", ["bin/lua/sys.lua"]);

							changeDirectory(miscDir + "luaDeadCode/stringReflection");
							runCommand("haxe", ["compile.hxml"]);
						}
					case Cpp:
						getCppDependencies();
						runCommand("haxe", ["compile-cpp.hxml", "-D", "HXCPP_M32"].concat(args));
						runCpp("bin/cpp/TestMain-debug", []);

						switch (ci) {
							case AppVeyor:
								//save time...
							case _:
								runCommand("rm", ["-rf", "cpp"]);
								runCommand("haxe", ["compile-cpp.hxml", "-D", "HXCPP_M64"].concat(args));
								runCpp("bin/cpp/TestMain-debug", []);

								runCommand("haxe", ["compile-cppia-host.hxml"]);
								runCommand("haxe", ["compile-cppia.hxml"]);
								runCpp("bin/cppia/Host-debug", ["bin/unit.cppia"]);
								runCpp("bin/cppia/Host-debug", ["bin/unit.cppia", "-jit"]);
								runCommand("haxe", ["compile-cppia.hxml", "-D", "nocppiaast"]);
								runCpp("bin/cppia/Host-debug", ["bin/unit.cppia"]);
								runCpp("bin/cppia/Host-debug", ["bin/unit.cppia", "-jit"]);
						}

						changeDirectory(sysDir);
						runCommand("haxe", ["compile-cpp.hxml"]);
						runCpp("bin/cpp/Main-debug", []);

						// if (Sys.systemName() == "Mac")
						// {
						// 	changeDirectory(miscDir + "cppObjc");
						// 	runCommand("haxe", ["build.hxml"]);
						// 	runCpp("bin/TestObjc-debug");
						// }
					case Js:
						getJSDependencies();

						var jsOutputs = [
							for (es3 in       [[], ["-D", "js-es=3"]])
							for (unflatten in [[], ["-D", "js-unflatten"]])
							for (classic in   [[], ["-D", "js-classic"]])
							for (enums_as_objects in [[], ["-D", "js-enums-as-objects"]])
							{
								var extras = args.concat(es3).concat(unflatten).concat(classic).concat(enums_as_objects);

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
								runCommand("node", ["-e", "require('./" + output + "').unit.TestMain.nodejsMain();"]);
								output;
							}
						];

						haxelibInstall("hxnodejs");
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
							runCommand("haxe", ["compile-saucelabs-runner.hxml"]);
							var server = new Process("nekotools", ["server"]);
							runCommand("node", ["bin/RunSauceLabs.js"].concat([for (js in jsOutputs) "unit-js.html?js=" + js.urlEncode()]));

							server.close();
							// sc.close();
						}

						infoMsg("Test optimization:");
						changeDirectory(optDir);
						runCommand("haxe", ["run.hxml"]);
						haxelibInstall("utest");
						changeDirectory(serverDir);
						runCommand("haxe", ["build.hxml"]);
						runCommand("node", ["test.js"]);
					case Java:
						getJavaDependencies();
						runCommand("haxe", ["compile-java.hxml"].concat(args));
						runCommand("java", ["-jar", "bin/java/TestMain-Debug.jar"]);

						runCommand("haxe", ["compile-java.hxml","-dce","no"].concat(args));
						runCommand("java", ["-jar", "bin/java/TestMain-Debug.jar"]);

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

						for (fastcast in      [[], ["-D", "fast_cast"]])
						for (noroot in        [[], ["-D", "no_root"]])
						for (erasegenerics in [[], ["-D", "erase_generics"]])
						{
							var extras = fastcast.concat(erasegenerics).concat(noroot);
							runCommand("haxe", ['compile-cs$compl.hxml'].concat(extras));
							runCs("bin/cs/bin/TestMain-Debug.exe");

							runCommand("haxe", ['compile-cs-unsafe$compl.hxml'].concat(extras));
							runCs("bin/cs_unsafe/bin/TestMain-Debug.exe");
						}

						runCommand("haxe", ['compile-cs$compl.hxml','-dce','no']);
						runCs("bin/cs/bin/TestMain-Debug.exe");

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
							var apacheMirror = Json.parse(Http.requestUrl("http://www.apache.org/dyn/closer.lua?as_json=1")).preferred;
							var flexVersion = "4.16.0";
							runCommand("wget", ['${apacheMirror}/flex/${flexVersion}/binaries/apache-flex-sdk-${flexVersion}-bin.tar.gz'], true);
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

		if (success) {
			deploy();
		} else {
			Sys.exit(1);
		}
	}
}



