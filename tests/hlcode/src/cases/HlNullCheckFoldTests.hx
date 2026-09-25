package cases;

private interface IBase {
	function base():Int;
}

private class Impl implements IBase {
	public function new() {}
	public function base():Int return 1;
}

private class Obj {
	public var v:Int;
	public function new() {}
}

class HlNullCheckFoldTests {
	static function compare(a:Obj, b:Obj):Int {
		return a.v - b.v;
	}

	@:hl(<>
		fun@N(Nh) (hl.types.ArrayObj):void
		r0 hl.types.ArrayObj
		r1 void
		r2 (cases._HlNullCheckFoldTests.Obj,cases._HlNullCheckFoldTests.Obj):i32
		r3 (dyn,dyn):i32
		@0 nullcheck 0
		@1 staticclosure 2, cases.HlNullCheckFoldTests.compare
		@2 instanceclosure 3, fun$N(2)
		@3 call 1, hl.types.ArrayObj.sort(0,3)
		@4 ret 1
	</>)
	static public function testClosureNotNull(a:Array<Obj>) {
		a.sort(compare);
	}

	@:hl(<>
		fun@N(Nh) ():virtual(base:method:():i32)
		r0 cases._HlNullCheckFoldTests.Impl
		r1 void
		r2 virtual(base:method:():i32)
		@0 new 0
		@1 call 1, cases._HlNullCheckFoldTests.Impl.new(0)
		@2 field 2,0[0]
		@3 jnotnull 2,2
		@4 tovirtual 2,0
		@5 setfield 0[0],2
		@6 ret 2
	</>)
	static public function testNewNotNull():IBase {
		return new Impl();
	}

	@:hl(<>
		fun@N(Nh) ():virtual(base:method:():i32)
		r0 cases._HlNullCheckFoldTests.Impl
		r1 virtual(base:method:():i32)
		@0 null 0
		@1 null 1
		@2 ret 1
	</>)
	static public function testNullSkipCast():IBase {
		var x:Impl = null;
		return x;
	}

	@:hl(<>
		fun@N(Nh) (type):i32
		r0 type
		r1 type
		r2 i32
		@0 type 1,cases._HlNullCheckFoldTests.Obj
		@1 jnoteq 0,1,2
		@2 int 2,@0
		@3 ret 2
		@4 int 2,@1
		@5 ret 2
	</>)
	static public function testUnusedNull(t:hl.Type):Int {
		if (t == hl.Type.get((null : Obj)))
			return 1;
		return 0;
	}
}
