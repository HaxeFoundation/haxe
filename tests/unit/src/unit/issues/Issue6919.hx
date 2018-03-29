package unit.issues;

class Issue6919 extends unit.Test {
	#if sys
	function test() {
		var req = new sys.Http("http://localhost").addHeader("Content-Type", "text/haxe");
		// this should be an Http, not an HttpBase
		req.responseHeaders;
		t(true);
	}
	#end
}
