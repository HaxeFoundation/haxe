package unit;

import utest.Async;

class TestHttp extends Test {
	#if flash
	public function setupClass() {
		flash.system.Security.allowDomain("*");
		flash.system.Security.allowInsecureDomain("*");
		flash.system.Security.loadPolicyFile("http://localhost:20200/crossdomain.xml");
	}
	#end

	static final RUN_HTTP_TESTS =
		#if !github false && #end // comment out line to run http tests locally
		#if (github && (java || flash || (cs && Windows)))
			false
		#elseif (js && !nodejs)
			js.Browser.supported
		#else
			true
		#end;

	function run(async:Async, test:()->Void) {
		if (RUN_HTTP_TESTS) {
			test();
			return;
		}
		noAssert();
		async.done();
	}

	@:timeout(1000)
	public function testPostData(async:Async) run(async, () -> {
		var srcStr = 'hello, world';
		var d = new haxe.Http('http://localhost:20200/echoServer.n');
		d.onData = echoStr -> {
			if(echoStr != srcStr) {
				assert('String data from Http request is corrupted: $echoStr');
			}
			noAssert();
			async.done();
		}
		d.onError = e -> {
			assert('Failed Http request with string data: $e');
			async.done();
		}
		d.setPostData(srcStr);
		d.request();
	});

	@:timeout(1000)
	public function testPostBytes(async:Async) run(async, () -> {
		var srcData = haxe.io.Bytes.alloc(256);
		for(i in 0...srcData.length) {
			srcData.set(i, i);
		}
		var d = new haxe.Http('http://localhost:20200/echoServer.n');
		d.onBytes = echoData -> {
			if(srcData.length != echoData.length) {
				assert('Binary data from Http request is corrupted. Wrong amount of bytes.');
			}
			for(i in 0...echoData.length) {
				switch [srcData.get(i), echoData.get(i)] {
					case [a, b] if(a != b):
						assert('Binary data from Http request is corrupted. Invalid byte value at index #$i: (src) $a != $b (echo)');
						break;
					case _:
				}
			}
			noAssert();
			async.done();
		}
		d.onError = e -> {
			assert('Failed Http request with binary data: $e');
			async.done();
		}
		d.setPostBytes(srcData);
		d.request();
	});
}
