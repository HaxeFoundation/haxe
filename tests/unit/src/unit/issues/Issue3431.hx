package unit.issues;

private typedef A = {
    ?f:Array<Int>,
}

class Issue3431 extends Test {
	function test() {
        var v:A = {};
        var r = switch (v) {
            case {f: [a,b]}: 1;
            default: 2;
        }
		eq(2, r);
	}
}