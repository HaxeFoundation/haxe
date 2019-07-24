package unit;

import utest.Async;

class TestHttp extends Test {
	public function setupClass() {
		#if flash
		flash.system.Security.allowDomain("*");
		flash.system.Security.loadPolicyFile("http://localhost:20200/crossdomain.xml");
		#end
	}

	function run(test:()->Void) {
		#if (js && !nodejs)
		if(js.Syntax.code('typeof XMLHttpRequest == "undefined"')) {
			noAssert();
			return;
		}
		#end
		test();
	}

	public function testPostData(async:Async) run(() -> {
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

	public function testPostBytes(async:Async) run(() -> {
		var srcData = haxe.io.Bytes.alloc(100);
		for(i in 0...srcData.length) {
			srcData.set(i, Std.random(256));
		}
		var d = new haxe.Http('http://localhost:20200/echoServer.n');
		d.onBytes = echoData -> {
			for(i in 0...echoData.length) {
				if(srcData.get(i) != echoData.get(i)) {
					assert('Binary data from Http request is corrupted');
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