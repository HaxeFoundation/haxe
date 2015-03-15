package unit.issues;

class Issue2980 extends Test {

	#if python
	function a(?args:python.KwArgs<Dynamic>) {}
    function b(?args:python.VarArgs<Dynamic>) {}

	function test() {
		a();
		b();
	}
	#end
}