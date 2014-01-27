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
		if (Sys.getEnv("TRAVIS") != null) tags.push("TravisCI");

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
			// {
			// 	"browserName": "internet explorer",
			// 	"platform": "Windows XP",
			// 	"version": "8"
			// },
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
				var caps = browsers.shift();
				caps.setField("name", "haxe");
				caps.setField("tags", tags);
				if (Sys.getEnv("TRAVIS") != null) {
					caps.setField("tunnel-identifier", Sys.getEnv("TRAVIS_JOB_NUMBER"));
					caps.setField("build", Sys.getEnv("TRAVIS_BUILD_NUMBER"));
				}

				console.log('========================================================');
				console.log('${caps.browserName} ${caps.version} on ${caps.platform}:');
				browser.init(caps, function() {
					browser.get("http://localhost:2000/unit-js.html", function() {
						browser.text("body", function(err, re) {
							if (err != null) throw err;
							console.log(re);
							browser.eval("unit.Test.success", function(err, re) {
								if (err != null) throw err;
								success = success && re;
								browser.sauceJobUpdate({ passed: re },function(err) {
									browser.quit(function(err) {
										if (err != null) throw err;
										testBrowsers(browsers);
									});
								});
							});
						});
					});
				});
			}
		}
		testBrowsers(browsers);
	}
}