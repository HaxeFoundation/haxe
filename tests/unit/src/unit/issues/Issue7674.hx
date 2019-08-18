package unit.issues;

private abstract MyAbstract({}) {
    public var abstractProperty (get, set):Array<Float>;
    public function new () { this = {}; }
    private inline function get_abstractProperty () { return []; }
    private inline function set_abstractProperty (value:Array<Float>) { return []; }
}

class Issue7674 extends unit.Test {
	function test() {
        var enumInstance = new MyAbstract ();
        enumInstance.abstractProperty = [1];
		utest.Assert.pass();
	}
}