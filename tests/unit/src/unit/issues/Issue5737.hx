package unit.issues;

#if python
private class C {
	public var v:Array<Int>;
	public var k:{a:Int};
	public function new(v:python.VarArgs<Int>, k:python.KwArgs<{a:Int}>) {
		this.v = v.toArray();
		this.k = k.typed();
	}
}
#end

class Issue5737 extends Test {
	#if python
	function test(){
		var c = new C([1,2,3], {a: 42});
		aeq(c.v, [1,2,3]);
		eq(c.k.a, 42);
	}
	#end
}
