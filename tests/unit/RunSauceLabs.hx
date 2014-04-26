import js.Node.*;
using Reflect;

class RunSauceLabs {
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
		var browsers = [
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
				"version": "31"
			},
			{
				"browserName": "firefox",
				"platform": "Windows XP",
				"version": "26"
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
				"platform": "OS X 10.8",
				"version": "6.1",
				"device-orientation": "portrait"
			},
			{
				"browserName": "iphone",
				"platform": "OS X 10.9",
				"version": "7",
				"device-orientation": "portrait"
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
							if (retries > 0)
								testBrowser(caps, retries - 1);
							else
								throw err;
							return false;
						}
						return true;
					}

					console.log('========================================================');
					console.log('${caps.browserName} ${caps.version} on ${caps.platform}:');
					browser.init(caps, function(err) {
						if (!handleError(err)) return;
						browser.get("http://localhost:2000/unit-js.html", function(err) {
							if (!handleError(err)) return;
							browser.text("body", function(err, re) {
								if (!handleError(err)) return;
								console.log(re);

								//check if test is successful or not
								var test = false;
								for (line in re.split("\n")) {
									if (line.indexOf("SUCCESS: ") >= 0) {
										test = line.indexOf("SUCCESS: true") >= 0;
										break;
									}
								}
								success = success && test;

								//let saucelabs knows the result
								browser.sauceJobUpdate({ passed: test }, function(err) {
									if (!handleError(err)) return;
									browser.quit(function(err) {
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
