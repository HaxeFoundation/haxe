package unit;

import utest.Async;

class TestHttps extends Test {
	static final RUN_HTTPS_TESTS =
		#if !github false && #end // comment out line to run https tests locally
		#if (github && flash)
			false
		#elseif (js && !nodejs)
			js.Browser.supported
		#else
			true
		#end;

	function run(async:Async, test:()->Void) {
		if (RUN_HTTPS_TESTS) {
			test();
			return;
		}
		noAssert();
		async.done();
	}

	// Check if http is working for this target before blaming https
	@:timeout(3000)
	public function testDownloadHttp(async:Async) run(async, () -> {
		final url = 'http://build.haxe.org/builds/haxe/linux64/haxe_latest.tar.gz';
		var req = new haxe.Http(url);
		req.onBytes = bytes -> {
			noAssert();
			async.done();
		}
		req.onError = e -> {
			assert('Failed Http request with string data: $e');
			async.done();
		}
		req.request();
	});

	@:timeout(3000)
	public function testDownloadHttps(async:Async) run(async, () -> {
		#if python
		if (Sys.systemName() == "Windows") {
			noAssert();
			return async.done();
		}
		#end

		final url = 'https://build.haxe.org/builds/haxe/linux64/haxe_latest.tar.gz';
		var req = new haxe.Http(url);
		req.onBytes = bytes -> {
			noAssert();
			async.done();
		}
		req.onError = e -> {
			assert('Failed Https request with string data: $e');
			async.done();
		}
		req.request();
	});
}
