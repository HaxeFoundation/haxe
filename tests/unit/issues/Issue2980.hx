package unit.issues;

class Issue2980 extends Test {

	#if python
	function a(?args:python.KwArgs) {}
    function b(?args:python.VarArgs) {}

	function test() {
		a();
		b();
	}
	#end
}