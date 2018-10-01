package runci.targets;

import sys.FileSystem;
import runci.System.*;
import runci.Config.*;
import haxe.Json;
import sys.io.File;
import haxe.Http;

class As3 {
	static public function run(args:Array<String>) {
		runci.targets.Flash.setupFlashPlayerDebugger();

		//setup flex sdk
		if (commandSucceed("mxmlc", ["--version"])) {
			infoMsg('mxmlc has already been installed.');
		} else {
			var apacheMirror = Json.parse(Http.requestUrl("http://www.apache.org/dyn/closer.lua?as_json=1")).preferred;
			var flexVersion = "4.16.0";
			runCommand("wget", ["-nv", '${apacheMirror}/flex/${flexVersion}/binaries/apache-flex-sdk-${flexVersion}-bin.tar.gz'], true);
			runCommand("tar", ["-xf", 'apache-flex-sdk-${flexVersion}-bin.tar.gz', "-C", Sys.getEnv("HOME")]);
			var flexsdkPath = Sys.getEnv("HOME") + '/apache-flex-sdk-${flexVersion}-bin';
			addToPATH(flexsdkPath + "/bin");
			var playerglobalswcFolder = flexsdkPath + "/player";
			FileSystem.createDirectory(playerglobalswcFolder + "/11.1");
			var flashVersion = runci.targets.Flash.getLatestFPVersion();
			runCommand("wget", ["-nv", 'http://download.macromedia.com/get/flashplayer/updaters/${flashVersion[0]}/playerglobal${flashVersion[0]}_${flashVersion[1]}.swc', "-O", playerglobalswcFolder + "/11.1/playerglobal.swc"], true);
			File.saveContent(flexsdkPath + "/env.properties", 'env.PLAYERGLOBAL_HOME=$playerglobalswcFolder');
			runCommand("mxmlc", ["--version"]);
		}

		runCommand("haxe", ["compile-as3.hxml", "-D", "fdb"].concat(args));
		var success = runci.targets.Flash.runFlash("bin/unit9_as3.swf");
		if (!success)
			fail();
	}
}