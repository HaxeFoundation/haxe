package runci;

import haxe.Timer;
import sys.io.Process;
import runci.Config.*;

using StringTools;

enum Failure {
	Fail;
}

class System {
	static public var success(default, null) = true;


	static public function successMsg(msg:String):Void {
		Sys.println('\x1b[32m' + msg + '\x1b[0m');
	}
	static public function failMsg(msg:String):Void {
		Sys.println('\x1b[31m' + msg + '\x1b[0m');
	}
	static public function infoMsg(msg:String):Void {
		Sys.println('\x1b[36m' + msg + '\x1b[0m');
	}

	static public function commandSucceed(cmd:String, args:Array<String>):Bool {
		return try {
			var p = new Process(cmd, args);
			var succeed = p.exitCode() == 0;
			p.close();
			succeed;
		} catch(e:Dynamic) false;
	}

	static public function commandResult(cmd:String, args:Array<String>):{
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

	/**
		Run a command using `Sys.command()`.
		If the command exits with non-zero code, exit the whole script with the same code.

		If `useRetry` is `true`, the command will be re-run if it exits with non-zero code (3 trials).
		It is useful for running network-dependent commands.
	*/
	static public function runCommand(cmd:String, ?args:Array<String>, useRetry:Bool = false, allowFailure:Bool = false):Void {
		var trials = useRetry ? 3 : 1;
		var exitCode:Int = 1;
		var cmdStr = cmd + (args == null ? '' : ' $args');

		while (trials-->0) {
			infoMsg('Command: $cmdStr');

			var t = Timer.stamp();
			exitCode = Sys.command(cmd, args);
			var dt = Math.round(Timer.stamp() - t);

			if (exitCode == 0) {
				successMsg('Command exited with $exitCode in ${dt}s: $cmdStr');
				return;
			}
			else
				failMsg('Command exited with $exitCode in ${dt}s: $cmdStr');

			if (trials > 0) {
				infoMsg('Command will be re-run...');
			}
		}

		if (!allowFailure)
			fail();
	}

	static public function fail():Void {
		success = false;
		throw Fail;
	}

	static public function addToPATH(path:String):Void {
		switch (systemName) {
			case "Windows":
				Sys.putEnv("PATH", path + ";" + Sys.getEnv("PATH"));
			case "Mac", "Linux":
				Sys.putEnv("PATH", path + ":" + Sys.getEnv("PATH"));
		}
	}

	static public function haxelibInstallGit(account:String, repository:String, ?branch:String, ?srcPath:String, useRetry:Bool = false, ?altName:String):Void {
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

	static public function haxelibInstall(library:String):Void {
		try {
			getHaxelibPath(library);
			infoMsg('$library has already been installed.');
		} catch (e:Dynamic) {
			runCommand("haxelib", ["install", library]);
		}
	}

	static public function haxelibRun(args:Array<String>, useRetry:Bool = false):Void {
		runCommand("haxelib", ["run"].concat(args), useRetry);
	}

	static public function getHaxelibPath(libName:String) {
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

	static public function changeDirectory(path:String) {
		Sys.println('Changing directory to $path');
		Sys.setCwd(path);
	}
}