package cases;

private interface IBase {
	function base():Int;
}

private interface IChild extends IBase {
	function child():Int;
}

private class Impl implements IChild {
	public function new() {}
	public function base():Int return 1;
	public function child():Int return 2;
}

class HlInterfaceCastTests {
	@:hl(<>
		fun@N(Nh) ():i32
		r0 cases._HlInterfaceCastTests.Impl
		r1 void
		r2 virtual(base:method:():i32)
		r3 virtual(base:method:():i32,child:method:():i32)
		r4 i32
		r5 i32
		.22    @0 new 0
		.22    @1 call 1, cases._HlInterfaceCastTests.Impl.new(0)
		.23    @2 jnotnull 0,2
		.23    @3 null 2
		.23    @4 jalways 4
		.23    @5 field 2,0[1]
		.23    @6 jnotnull 2,2
		.23    @7 tovirtual 2,0
		.23    @8 setfield 0[1],2
		.24    @9 jnotnull 0,2
		.24    @A null 3
		.24    @B jalways 4
		.24    @C field 3,0[0]
		.24    @D jnotnull 3,2
		.24    @E tovirtual 3,0
		.24    @F setfield 0[0],3
		.25    @10 nullcheck 2
		.25    @11 callmethod 4, 2[0]()
		.25    @12 nullcheck 3
		.25    @13 callmethod 5, 3[1]()
		.25    @14 add 4,4,5
		.25    @15 ret 4
	</>)
	static public function testParentAndChildVirtualSlots() {
		var obj = new Impl();
		var base:IBase = obj;
		var child:IChild = obj;
		return base.base() + child.child();
	}
}
