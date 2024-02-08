package unit.issues;
private typedef MyWrapper<T, C> = {val1:T, val2:C, name:String};

@:forward
private abstract MyType<T, C:Iterable<T>>(MyWrapper<T, C>) from MyWrapper<T, C> {
    @:op(a < b)
    static public inline function test<U, D:Iterable<U>>(a:MyType<U, D>, b:MyType<U, D>) : Bool {
        return a.name.charCodeAt(0) < b.name.charCodeAt(0);
    }
}

class Issue9123 extends Test {
	function test() {
		var c:MyType<Int, Array<Int>> = { val1:0, val2:[0], name:"a" },
		d:MyType<Int, Array<Int>> = { val1:1, val2:[1], name:"b" };

		t(c < d);
		t(MyType.test(c, d));
	}
}