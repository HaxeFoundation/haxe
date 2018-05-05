package unit.issues;

class Issue5764 extends unit.Test {
	@:analyzer(no_local_dce)
	function test() {
        var f:Float;
        f = 0;
        --f;
		feq(-1., f);
	}
}