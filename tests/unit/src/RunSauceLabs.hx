import js.Node.*;
using Reflect;
using Lambda;

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
		var allSuccess = true;
		var q:Dynamic = require("q");
		var webdriver:Dynamic = require("wd");
		var browser:Dynamic = webdriver.promiseRemote(
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
				"platform": "Windows XP"
			},
			{
				"browserName": "firefox",
				"platform": "Windows XP"
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
				"browserName": "safari",
				"platform": "OS X 10.10",
				"version": "8"
			},
			{
				"browserName": "iphone",
				"platform": "OS X 10.8",
				"version": "6.1",
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

		var timeout = 30000; //30s

		function testBrowser(caps:Dynamic, trials = 3):Dynamic {
			console.log('========================================================');
			console.log('Requesting: ${caps.browserName} ${caps.version} on ${caps.platform}');

			caps.setField("name", Sys.getEnv("TRAVIS") != null ? Sys.getEnv("TRAVIS_REPO_SLUG") : "haxe");
			caps.setField("tags", tags);
			if (Sys.getEnv("TRAVIS") != null) {
				caps.setField("tunnel-identifier", Sys.getEnv("TRAVIS_JOB_NUMBER"));
				caps.setField("build", Sys.getEnv("TRAVIS_BUILD_NUMBER"));
			}

			trials--;

			function onErrored(err):Dynamic {
				console.log(err);
				if (trials > 0) {
					return browser
						.sauceJobUpdate({ passed: true, tags: tags.concat(["errored"]) })
						.then(function() return browser.quit())
						.timeout(timeout)
						.fail(onErrored)
						.then(function() return testBrowser(caps, trials));
				} else {
					allSuccess = false;
					return null;
				}
			}

			function until(code:String) {
				return browser
					.execute(code)
					.then(function(v)
						if (v)
							return null;
						else
							return q.delay(1000)
								.then(function() return until(code))
					);
			}

			return browser
				.init(caps)
				.then(function() {
					return browser
						.sessionCapabilities()
						.then(function(caps) {
							console.log('Using: ${caps.browserName} ${caps.version} on ${caps.platform}');
						});
				})
				.then(function()
					return browser.setAsyncScriptTimeout(timeout))
				.then(function(){
					console.log("[debug] opening test page");
					return browser.get("http://localhost:2000/unit-js.html");
				})
				.then(function() {
					console.log("[debug] waiting for test to exit");
					return 
						until("return (typeof unit != 'undefined') && unit.Test && (typeof unit.Test.success === 'boolean')")
						.timeout(timeout);
				})
				.then(function() {
					console.log("[debug] test exited");
					return browser.text("body");
				})
				.then(function(resultText:String) {
					//check if test is successful or not
					var success = false;
					for (line in resultText.split("\n")) {
						infoMsg(line);
						if (line.indexOf("SUCCESS: ") >= 0) {
							success = line.indexOf("SUCCESS: true") >= 0;
							break;
						}
					}
					allSuccess = allSuccess && success;

					if (success) {
						successMsg('${caps.browserName} ${caps.version} on ${caps.platform}: SUCCESS');
					} else {
						failMsg('${caps.browserName} ${caps.version} on ${caps.platform}: FAIL');
					}

					return browser.sauceJobUpdate({ passed: success });
				})
				.then(function()
					return browser.quit())
				.timeout(60000 * 5) //5 min
				.fail(onErrored);
		}

		browsers
			.fold(function(caps:Dynamic, promise:Dynamic):Dynamic {
				return promise.then(function () return testBrowser(caps));
			}, q())
			.then(function() {
				Sys.exit(allSuccess ? 0 : 1);
			});
	}
}
