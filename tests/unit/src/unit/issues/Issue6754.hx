package unit.issues;

class Issue6754 extends Test {
	#if js
	function test() {
		try throw "hello" catch (e:Any) eq(js.Lib.getOriginalException().message, "hello");
	}
	#end
}
