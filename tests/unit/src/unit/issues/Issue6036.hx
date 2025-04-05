package unit.issues;

using Lambda;

private class TYPE {
    public function new() {}
    public function iterator():Iterator<Int> return new IntIterator(0, 1);
}

private abstract ABSTRACT(TYPE) {
    public var array(get, never):Bool;
    function get_array() return true;

    public function new() {
        this = new TYPE();
	}

	public function getArray() {
        return array;
	}

	public function getThisArray() {
        return this.array;
    }
}

class Issue6036 extends unit.Test {
	function test() {
		var a = new ABSTRACT();
		HelperMacros.typedAs(a.getArray(), true);
		HelperMacros.typedAs(a.getThisArray(), (null : () -> Array<Int>));
	}
}