import haxe.Constraints;
import haxe.*;
import js.Node.*;
using Reflect;
using Lambda;

/**
	Promise interface of [Q](https://github.com/kriskowal/q).
	Incomplete.
*/
private typedef Promise = {
	public function then(onResolve:Function, ?onReject:Function):Promise;
	public function fail(onReject:Function):Promise;
}

/**
	Run tests on SauceLabs.

	Usage: npm install wd q && node RunSauceLabs.js testFile.html [...testFile2.html] [options]
	Options:
		-serve-domain <domain>		Domain of the server that serves the test files. Default: localhost
		-serve-port <port>			Port number of the server that serves the test files. Default: 2000
		-connect-domain <domain>	Domain of the remote testing server. Default: localhost (using Sauce Connect)
		-connect-port <port>		Port number of remote testing server. Default: 4445 (using Sauce Connect)
		-browsers <browsers>		A list of browsers to test with in JSON format. Default: refer to source code

	When a test finishes, it should set `window.success` to a boolean.
*/
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
		var serveDomain = "localhost";
		var servePort = "2000";
		var connectDomain = "localhost";
		var connectPort = "4445";
		var urls = [];

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
				"browserName": "iphone",
				"platform": "OS X 10.9",
				"version": "8.1",
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

		var arg, args = process.argv.slice(2);
		while ((arg = args.shift()) != null) {
			switch (arg) {
				case "-serve-domain":
					serveDomain = args.shift();
				case "-serve-port":
					servePort = args.shift();
				case "-connect-domain":
					connectDomain = args.shift();
				case "-connect-port":
					connectPort = args.shift();
				case "-browsers":
					browsers = Json.parse(args.shift());
				case _:
					urls.push(arg);
			}
		}

		var allSuccess = true;
		var q:Dynamic = require("q");
		var webdriver:Dynamic = require("wd");
		var browser:Dynamic = webdriver.promiseRemote(
			connectDomain,
			connectPort,
			Sys.getEnv("SAUCE_USERNAME"),
			Sys.getEnv("SAUCE_ACCESS_KEY")
		);

		var tags = [];
		if (Sys.getEnv("TRAVIS") != null)
			tags.push("TravisCI");

		var maxDuration = 60 * 5; //5 min
		var commandTimeout = 30;  //30s

		function testBrowser(caps:Dynamic, trials = 3):Dynamic {
			console.log('========================================================');
			var browserName = caps.hasField("version") ? '${caps.browserName} ${caps.version}' : caps.browserName;
			console.log('Requesting: ${browserName} on ${caps.platform}');

			caps.setField("name", Sys.getEnv("TRAVIS") != null ? Sys.getEnv("TRAVIS_REPO_SLUG") : "haxe");
			caps.setField("tags", tags);
			caps.setField("maxDuration", maxDuration);
			caps.setField("commandTimeout", commandTimeout);
			caps.setField("avoidProxy", true);
			if (Sys.getEnv("TRAVIS") != null) {
				caps.setField("tunnel-identifier", Sys.getEnv("TRAVIS_JOB_NUMBER"));
				caps.setField("build", Sys.getEnv("TRAVIS_BUILD_NUMBER"));
			}

			trials--;

			function onErrored(err):Dynamic {
				console.log(Std.string(err));
				console.log("detail: " + Json.stringify(err, "  "));
				if (trials > 0) {
					return browser
						.sauceJobUpdate({ passed: true, tags: tags.concat(["errored"]) })
						.then(function() return browser.quit())
						.timeout(commandTimeout * 1000)
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

			var browserSuccess = true;

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
					return browser.setAsyncScriptTimeout(commandTimeout * 1000))
				.then(function(){
					return urls.fold(function(url:String, promise:Promise):Promise {
						return promise.then(function(){
							var url = 'http://${serveDomain}:${servePort}/${url}';
							console.log('[debug] opening $url');
							return browser.get(url)
								.then(function() {
									console.log("[debug] waiting for test to exit");
									return
										until("return (typeof window.success === 'boolean');")
										.timeout(commandTimeout * 1000);
								})
								.then(function() {
									console.log("[debug] test exited");
									return browser.text("body");
								})
								.then(function(text:String) {
									text.split("\n").iter(infoMsg);
								})
								.then(function() {
									return browser
										.execute("return window.success;")
										.then(function(success) {
											browserSuccess = browserSuccess && success;
											allSuccess = allSuccess && browserSuccess;

											if (success) {
												successMsg('$url in ${caps.browserName} ${caps.version} on ${caps.platform}: SUCCESS');
											} else {
												failMsg('$url in ${caps.browserName} ${caps.version} on ${caps.platform}: FAIL');
											}
										});
								})
								.timeout(maxDuration * 1000);
						});
					}, q());
				})
				.then(function()
					return browser.sauceJobUpdate({ passed: browserSuccess }))
				.then(function()
					return browser.quit())
				.fail(onErrored);
		}

		browsers
			.fold(function(caps:Dynamic, promise:Promise):Promise {
				return promise.then(function () return testBrowser(caps));
			}, q())
			.then(function() {
				Sys.exit(allSuccess ? 0 : 1);
			});
	}
}
