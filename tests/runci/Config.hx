package runci;

import sys.FileSystem;
import haxe.io.Path;

enum Ci {
	GithubActions;
}

class Config {
	static public final systemName = Sys.systemName();
	static public final cwd = Sys.getCwd();
	static public final repoDir = FileSystem.fullPath("..") + "/";
	static public final unitDir = Path.join([cwd, "unit"]);
	static public final sysDir = Path.join([cwd, "sys"]);
	static public final optDir = cwd + "optimization/";
	static public final displayDir = Path.join([cwd, "display"]);
	static public final serverDir = Path.join([cwd, "server"]);
	static public final sourcemapsDir = Path.join([cwd, "sourcemaps"]);
	static public final nullSafetyDir = Path.join([cwd, "nullsafety"]);
	static public final threadsDir = Path.join([cwd, "threads"]);
	// this stands for third-party, but I've always wanted a party directory
	static public final partyDir = Path.join([cwd, "party"]);

	static public function getMiscSubDir(...subDir:String)
		return Path.join([cwd, "misc"].concat(subDir.toArray()));

	static public final ci:Null<Ci> =
		if (Sys.getEnv("GITHUB_ACTIONS") == "true")
			GithubActions;
		else
			null;

	static public macro function isCi() {
		return macro $v{ci != null};
	}

	static public final colorSupported = switch [ci, systemName] {
		case [GithubActions, _]: true;
		case [_, "Linux" | "Mac"]: true;
		case [_, "Windows"]: false;
		case _: false;
	}
}
