import js.Node.*;
using Reflect;

class RunSauceLabs {
	static function successMsg(msg:String):Void {
		console.log('\x1b[32m' + msg + '\x1b[0m');
	}
	static function failMsg(msg:String):Void {
		console.log('\x1b[31m' + msg + '\x1b[0m');
	}
	static function infoMsg(msg:String):Void {
		console.log('\x1b[36m' + msg + '\x1b[0m');
	}

	static function main():Void {
		var success = true;
		var webdriver:Dynamic = require("wd");
		var browser:Dynamic = webdriver.remote(
			"localhost",
			4445,
			Sys.getEnv("SAUCE_USERNAME"),
			Sys.getEnv("SAUCE_ACCESS_KEY")
		);

		var tags = [];
		if (Sys.getEnv("TRAVIS") != null)
			tags.push("TravisCI");

		//https://saucelabs.com/platforms
		var browsers:Array<Dynamic> = [
			// {
			// 	"browserName": "internet explorer",
			// 	"platform": "Windows XP",
			// 	"version": "6"
			// },
			// {
			// 	"browserName": "internet explorer",
			// 	"platform": "Windows XP",
			// 	"version": "7"
			// },
			{
				"browserName": "internet explorer",
				"platform": "Windows XP",
				"version": "8"
			},
			{
				"browserName": "internet explorer",
				"platform": "Windows 7",
				"version": "9"
			},
			{
				"browserName": "internet explorer",
				"platform": "Windows 7",
				"version": "10"
			},
			{
				"browserName": "internet explorer",
				"platform": "Windows 8.1",
				"version": "11"
			},
			{
				"browserName": "chrome",
				"platform": "Windows XP",
				"version": "35"
			},
			{
				"browserName": "firefox",
				"platform": "Windows XP",
				"version": "30"
			},
			{
				"browserName": "safari",
				"platform": "OS X 10.6",
				"version": "5"
			},
			{
				"browserName": "safari",
				"platform": "OS X 10.8",
				"version": "6"
			},
			{
				"browserName": "safari",
				"platform": "OS X 10.9",
				"version": "7"
			},
			{
				"browserName": "iphone",
				"platform": "OS X 10.9",
				"version": "7.1",
				"device-orientation": "portrait"
			},
			{
				"browserName": "android",
				"platform": "Linux",
				"version": "4.0",
				"device-orientation": "portrait"
			},
			{
				"browserName": "android",
				"platform": "Linux",
				"version": "4.3",
				"device-orientation": "portrait"
			}
		];

		function testBrowsers(browsers:Array<Dynamic>) {
			if (browsers.length == 0) {
				Sys.exit(success ? 0 : 1);
			} else {
				function testBrowser(caps:Dynamic, retries = 3):Void {
					function handleError(err:String, ?pos:haxe.PosInfos):Bool {
						if (err != null) {
							console.log('${pos.fileName}:${pos.lineNumber}: $err');
							browser.quit(function(err) {
								if (retries > 0)
									testBrowser(caps, retries - 1);
								else
									throw err;
							});
							return false;
						}
						return true;
					}

					console.log('========================================================');
					console.log('${caps.browserName} ${caps.version} on ${caps.platform}:');
					browser.init(caps, function(err) {
						if (!handleError(err)) return;
						browser.setAsyncScriptTimeout(30000); //10s timeout
						browser.get("http://localhost:2000/unit-js.html", function(err) {
							if (!handleError(err)) return;

							console.log("[debug] waiting for test exit");
							browser.waitForConditionInBrowser("try { typeof unit.Test.success === 'boolean'; } catch(e) { false; }", 15000); //15s timeout
							console.log("[debug] test exited");

							browser.text("body", function(err, re:String) {
								if (!handleError(err)) return;

								//check if test is successful or not
								var test = false;
								for (line in re.split("\n")) {
									infoMsg(line);
									if (line.indexOf("SUCCESS: ") >= 0) {
										test = line.indexOf("SUCCESS: true") >= 0;
									}
								}
								success = success && test;

								if (test) {
									successMsg('${caps.browserName} ${caps.version} on ${caps.platform}: SUCCESS');
								} else {
									failMsg('${caps.browserName} ${caps.version} on ${caps.platform}: FAIL');
								}

								//let saucelabs knows the result
								browser.sauceJobUpdate({ passed: test }, function(err) {
									console.log("[debug] job update: " + (err == null ? "ok" : err));
									if (!handleError(err)) return;
									browser.quit(function(err) {
										console.log("[debug] browser quit: " + (err == null ? "ok" : err));
										if (!handleError(err)) return;
										testBrowsers(browsers);
									});
								});
							});
						});
					});
				}

				var caps = browsers.shift();
				caps.setField("name", Sys.getEnv("TRAVIS") != null ? Sys.getEnv("TRAVIS_REPO_SLUG") : "haxe");
				caps.setField("tags", tags);
				if (Sys.getEnv("TRAVIS") != null) {
					caps.setField("tunnel-identifier", Sys.getEnv("TRAVIS_JOB_NUMBER"));
					caps.setField("build", Sys.getEnv("TRAVIS_BUILD_NUMBER"));
				}
				testBrowser(caps);
			}
		}
		testBrowsers(browsers);
	}
}
