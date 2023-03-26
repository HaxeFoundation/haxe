package runci;

import haxe.Timer;
import sys.io.Process;
import runci.Config.*;

using StringTools;

class CommandFailure extends haxe.Exception {
	public final exitCode:Int;
	public function new(exitCode:Int = 1) {
		super("Command failed: " + Std.string(exitCode));
		this.exitCode = exitCode;
	}
}

class System {
	static public function successMsg(msg:String):Void {
		Sys.println(colorSupported ? '\x1b[32m' + msg + '\x1b[0m' : msg);
	}
	static public function failMsg(msg:String):Void {
		Sys.println(colorSupported ? '\x1b[31m' + msg + '\x1b[0m' : msg);
	}
	static public function infoMsg(msg:String):Void {
		Sys.println(colorSupported ? '\x1b[36m' + msg + '\x1b[0m' : msg);
	}

	static public function commandSucceed(cmd:String, args:Array<String>):Bool {
		return try {
			final p = new Process(cmd, args);
			final succeed = p.exitCode() == 0;
			p.close();
			succeed;
		} catch(e:Dynamic) false;
	}

	static public function commandResult(cmd:String, args:Array<String>):{
		stdout:String,
		stderr:String,
		exitCode:Int
	} {
		final p = new Process(cmd, args);
		final out = {
			stdout: p.stdout.readAll().toString(),
			stderr: p.stderr.readAll().toString(),
			exitCode: p.exitCode()
		}
		p.close();
		return out;
	}

	static inline function getDisplayCmd(cmd:String, ?args:Array<String>) {
		return cmd + (args == null ? '' : ' $args');
	}

	/**
		Run a command using `Sys.command()`.
		If the command exits with non-zero code, throws `CommandFailure` with the same code.
	*/
	static public function runCommand(cmd:String, ?args:Array<String>):Void {
		final exitCode = showAndRunCommand(cmd, args);

		if (exitCode != 0)
			throw new CommandFailure(exitCode);
	}

	/** Runs command using `Sys.command()` but ignores failures. **/
	static public function attemptCommand(cmd:String, ?args:Array<String>) {
		showAndRunCommand(cmd, args);
	}

	static function showAndRunCommand(cmd:String, args:Null<Array<String>>, ?displayed:String):Int {
		if (displayed == null)
			displayed = getDisplayCmd(cmd, args);

		infoMsg('Command: $displayed');

		final t = Timer.stamp();
		final exitCode = Sys.command(cmd, args);
		final dt = Math.round(Timer.stamp() - t);

		final msg = 'Command exited with $exitCode in ${dt}s: $displayed';
		if (exitCode != 0)
			failMsg(msg);
		else
			successMsg(msg);

		return exitCode;
	}


	static final TRIALS = 3;
	/**
		Run a command using `Sys.command()` with up to three attempts. Useful for running network-dependent commands.

		If all three attempts fail, throws a `CommandFailure` with the exit code of the last run.
	**/
	static public function runNetworkCommand(cmd:String, ?args:Array<String>) {
		final cmdStr = getDisplayCmd(cmd, args);

		for (trial in 1...TRIALS+1){
			final exitCode = showAndRunCommand(cmd, args, cmdStr);
			if (exitCode == 0)
				return;

			if (trial == TRIALS)
				throw new CommandFailure(exitCode);

			infoMsg('Command will be re-run...');
		}
	}

	/**
	 * Recursively delete a directory.
	 * @return Int Exit code of a system command executed to perform deletion.
	 */
	static public function deleteDirectoryRecursively(dir:String):Int {
		return switch (Sys.systemName()) {
			case "Windows":
				Sys.command("rmdir", ["/S", "/Q", StringTools.replace(sys.FileSystem.fullPath(dir), "/", "\\")]);
			case _:
				Sys.command("rm", ["-rf", dir]);
		}
	}

	static public function addToPATH(path:String):Void {
		infoMsg('Prepending $path to PATH.');
		switch (systemName) {
			case "Windows":
				Sys.putEnv("PATH", path + ";" + Sys.getEnv("PATH"));
			case "Mac", "Linux":
				Sys.putEnv("PATH", path + ":" + Sys.getEnv("PATH"));
		}
	}

	static function isLibraryInstalled(library:String):Bool {
		return new Process("haxelib", ["path", library]).exitCode() == 0;
	}

	static public function haxelibInstallGit(account:String, repository:String, ?branch:String, ?srcPath:String, useRetry:Bool = false, ?altName:String):Void {
		final name = (altName == null) ? repository : altName;
		// It could only be installed already if we're on a local machine
		if (!isCi() && isLibraryInstalled(name))
			return infoMsg('$name has already been installed.');

		final args = ["git", name, 'https://github.com/$account/$repository'];
		if (branch != null)
			args.push(branch);
		if (srcPath != null)
			args.push(srcPath);

		if (useRetry)
			runNetworkCommand("haxelib", args);
		else
			runCommand("haxelib", args);
	}

	static public function haxelibInstall(library:String):Void {
		// It could only be installed already if we're on a local machine
		if (!isCi() && isLibraryInstalled(library))
			return infoMsg('$library has already been installed.');
		runCommand("haxelib", ["install", library]);
	}

	static public function haxelibDev(library:String, path:String):Void {
		// It could only be installed already if we're on a local machine
		if (!isCi() && isLibraryInstalled(library))
			return infoMsg('$library has already been installed.');
		runCommand("haxelib", ["dev", library, path]);
	}

	static public function haxelibRun(args:Array<String>, useRetry:Bool = false):Void {
		final allArgs = ["run"].concat(args);
		if (useRetry)
			runNetworkCommand("haxelib", allArgs);
		else
			runCommand("haxelib", allArgs);
	}

	static public function getHaxelibPath(libName:String) {
		final proc = new Process("haxelib", ["path", libName]);
		final code = proc.exitCode();
		var result;
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

	static function mergeArgs(cmd:String, args:Array<String>) {
		return switch (Sys.systemName()) {
			case "Windows":
				[StringTools.replace(cmd, "/", "\\")].concat(args).map(haxe.SysTools.quoteWinArg.bind(_, true)).join(" ");
			case _:
				[cmd].concat(args).map(haxe.SysTools.quoteUnixArg).join(" ");
		}
	}

	/* command for setting the environment variable to set before sys tests */
	static final setCommand = if (Sys.systemName() == "Windows") "set"; else "export";
	static final nameAndValue = "EXISTS=1";

	/** Prepares environment for system tests and runs `cmd` with `args` **/
	static public function runSysTest(cmd:String, ?args:Array<String>) {
		final toDisplay = getDisplayCmd(setCommand, [nameAndValue]) + ' && ' + getDisplayCmd(cmd, args);

		if (args != null)
			cmd = mergeArgs(cmd, args);

		final fullCmd = '$setCommand $nameAndValue && $cmd';

		final exitCode = showAndRunCommand(fullCmd, null, toDisplay);

		if (exitCode != 0)
			throw new CommandFailure(exitCode);
	}

	static final installPath = if (systemName == "Windows")
			Sys.getEnv("USERPROFILE") + "/haxe-ci";
		else
			Sys.getEnv("HOME") + "/haxe-ci";

	/** Returns path where packages should be installed. **/
	public static inline function getInstallPath():String {
		return installPath;
	}

	/** Returns path where downloads should be placed. **/
	public static inline function getDownloadPath():String {
		return installPath + "/downloads";
	}

}
