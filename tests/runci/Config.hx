package runci;

import sys.FileSystem;

enum Ci {
	TravisCI;
	AppVeyor;
}

class Config {
	static public final systemName = Sys.systemName();
	static public final cwd = Sys.getCwd();
	static public final repoDir = FileSystem.fullPath("..") + "/";
	static public final unitDir = cwd + "unit/";
	static public final sysDir = cwd + "sys/";
	static public final optDir = cwd + "optimization/";
	static public final miscDir = cwd + "misc/";
	static public final displayDir = cwd + "display/";
	static public final serverDir = cwd + "server/";
	static public final sourcemapsDir = cwd + "sourcemaps/";

	static public final ci:Null<Ci> =
		if (Sys.getEnv("TRAVIS") == "true")
			TravisCI;
		else if (Sys.getEnv("APPVEYOR") == "True")
			AppVeyor;
		else
			null;
}