package runci;

import runci.Config.*;
import runci.System.*;
import sys.io.File;
import sys.FileSystem;

import haxe.Template;
using StringTools;

class Deployment {
	static var S3_HXBUILDS_ADDR(default, null) = 's3://hxbuilds/builds/haxe';

	static var gitInfo(get, null):{repo:String, branch:String, commit:String, timestamp:Float, date:String};

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

	static function isDeployNightlies () {
		return Sys.getEnv("DEPLOY_NIGHTLIES") != null;
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
			switch (ci) {
				case null:
					trace('Not deploying nightlies (not in CI)');
				case TravisCI:
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
				case AppVeyor:
					var kind = switch (Sys.getEnv("ARCH")) {
						case null:
							throw "ARCH is not set";
						case "32":
							"windows";
						case "64":
							"windows64";
						case _:
							throw "unknown ARCH";
					}

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
								submitToS3(kind, 'out/$file');
							} else if (file.endsWith('_installer.zip')) {
								submitToS3('${kind}-installer', 'out/$file');
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

	static function createNsiInstaller() {
		if (!FileSystem.exists('installer')) {
			FileSystem.createDirectory('installer');
		}
		getLatestNeko();
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

	static public function deploy():Void {
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
}