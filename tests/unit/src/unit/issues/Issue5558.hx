package unit.issues;

private class O {
    public var f(default,null):Int;
    public function new() f = 1;
    public function mutate() f = 2;
}

class Issue5558 extends unit.Test {
	function test() {
        fun(new O());
    }

    function fun(o:O) {
        var oldF = o.f;
        o.mutate();
        eq(1, oldF);
    }
}