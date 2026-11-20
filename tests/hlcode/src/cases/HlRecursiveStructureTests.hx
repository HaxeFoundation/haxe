package cases;

private typedef List1 = {v:Int, next:List1};
private typedef List2 = {v:Int, next:{v:Int, next:List2}};
private typedef Wrapper = {tests:HlRecursiveStructureTests, kind:Kind, next:Wrapper};

private class ListHolder {
	public var list:List1;

	public function new() {}
}

private class WrapperHolder {
	public var wrapper:Wrapper;

	public function new() {}
}

private enum Kind {
	KList(list:List2);
}

class HlRecursiveStructureTests {
	@:hl(<>
		fun@N(Nh) (cases.HlRecursiveStructureTests):void
		r0 cases.HlRecursiveStructureTests
		r1 ():virtual(next:...,v:i32)
		r2 void
		@0 getthis 1,[0]
		@1 jnotnull 1,2
		@2 instanceclosure 1, cases.HlRecursiveStructureTests.getList(0)
		@3 setthis [0],1
		@4 ret 2
	</>)
	public function new() {}

	public dynamic function getList():List2 {
		return null;
	}

	@:hl(<>
		fun@N(Nh) (enum(cases._HlRecursiveStructureTests.Kind)):virtual(next:...,v:i32)
		r0 enum(cases._HlRecursiveStructureTests.Kind)
		r1 virtual(next:...,v:i32)
		@0 nullcheck 0
		@1 enumfield 1, 0[0:0]
		@2 ret 1
	</>)
	static public function testEnumReachedFromStructure(kind:Kind) {
		return switch kind {
			case KList(list): list;
		}
	}
}
