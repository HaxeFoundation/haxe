package unit.issues;
import unit.Test;

private abstract A(Array<Int>) {
    public inline function new(a) {
        this = a;
    }
    @:arrayAccess inline function read(i) {
        return this[i];
    }
    @:arrayAccess inline function write(i,v) {
        return this[i] = v;
    }
}

class Issue2810 extends Test {
	function test() {
        var a = new A([5]);
        var x = a[0];
        a[0] = 6;
        a[0] += 3;
		eq(9, a[0]);
	}
}