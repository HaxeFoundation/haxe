package unit;

import utest.Async;

class TestHttp extends Test {
	public function setupClass() {
		#if flash
		flash.system.Security.allowDomain("*");
		flash.system.Security.allowInsecureDomain("*");
		flash.system.Security.loadPolicyFile("http://localhost:20200/crossdomain.xml");
		#end
	}

	function run(async:Async, test:()->Void) {
		// { comment this out to run http tests locally
		#if (!github || (github && js && !nodejs)) //also don't run on sauce labs
		noAssert();
		async.done();
		return;
		#end
		// }

		#if (js && !nodejs)
		if(!js.Browser.supported) {
			noAssert();
			async.done();
			return;
		}
		test();
		#elseif (github && (java || (flash && (Linux || Mac || Windows)) || (cs && Windows)))
		noAssert();
		async.done();
		return;
		#else
		test();
		#end
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
