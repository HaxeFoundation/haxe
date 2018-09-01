package unit.issues;

class Issue3539 extends unit.Test {
	function test() {
		var s = "";
		for (i in (10 : To100)) {
            s += i;
        }
		eq("10111213141516171819", s);
	}
}

private abstract To100(Int) from Int {
    public inline function new(start:Int) this = start;
    public inline function hasNext() : Bool return this < 20;
    public inline function next() : Int {
        var next = this;
        this = this + 1;
        return next;
    }
}