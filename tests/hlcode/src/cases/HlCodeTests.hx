package cases;

private typedef TreeA = {
	var ?node : TreeA;
}

private enum EKind {
	Empty;
}

private typedef ApplicationDesc = {
	@:optional final target:EKind;
}

private typedef WheelGroup = {
	?choices : Array<String>,
	?neutralChoice: String,
}

private class ChoiceWheel {
	public function new( group : WheelGroup ) {
	}
}

/**
	Tests that verify correct HL code generation for basic patterns.
**/
class HlCodeTests {
	/**
		Test that an anonymous object with an optional enum field is initialized correctly.
		The expected HL output verifies the struct allocation and field assignment.
	**/
	@:hl(<>
		fun@364(16Ch) ():virtual(target:enum(cases._HlCodeTests.EKind))
		; src/cases/HlCodeTests.hx:30 (cases.HlCodeTests.registerAffixDesc)
			r0 virtual(target:enum(cases._HlCodeTests.EKind))
			r1 enum(cases._HlCodeTests.EKind)
			.30    @0 new 0
			.30    @1 global 1, 18
			.30    @2 setfield 0[0],1
			.30    @3 ret 0
	</>)
	static public function registerAffixDesc():ApplicationDesc {
		return {target: Empty};
	}

	@:hl(<>
		fun@366(16Eh) ():void
		; src/cases/HlCodeTests.hx:44 (cases.HlCodeTests.testWheelGroup)
		r0 hl.types.ArrayObj
		r1 i32
		r2 array(dyn)
		r3 array(String)
		r4 type
		r5 void
		r6 cases._HlCodeTests.ChoiceWheel
		r7 dynobj
		r8 virtual(choices:hl.types.ArrayObj,neutralChoice:String)
		.44    @0 int 1,@0
		.44    @1 type 4,String
		.44    @2 call 2, std@alloc_array(4,1)
		.44    @3 unsafecast 3,2
		.44    @4 call 0, hl.types.ArrayObj.alloc(3)
		.45    @5 new 6
		.45    @6 new 7
		.46    @7 dynset 7[@159],0
		.46    @8 tovirtual 8,7
		.45    @9 call 5, cases._HlCodeTests.ChoiceWheel.new(6,8)
		.48    @A ret 5
	</>)
	static public function testWheelGroup() {
		var icons : Array<String> = [];
		new ChoiceWheel({
			choices: icons,
		});
	}

	@:hl(<>
		fun@367(16Fh) ():virtual(node:...)
		; src/cases/HlCodeTests.hx:82 (cases.HlCodeTests.testTreeA)
			r0 virtual(node:virtual(node:...))
			r1 virtual(node:...)
			.82    @0 new 0
			.82    @1 null 1
			.82    @2 setfield 0[0],1
			.83    @3 ret 0
	</>)
	static public function testTreeA() {
		var a : TreeA = { node : null };
		return a;
	}
}
