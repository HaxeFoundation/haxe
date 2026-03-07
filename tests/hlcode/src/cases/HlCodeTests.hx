package cases;

enum EKind {
	Empty;
}

typedef ApplicationDesc = {
	@:optional final target:EKind;
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
		fun@364(16Ch) ():virtual(target:enum(cases.EKind))
		; src/cases/HlCodeTests.hx:30 (cases.HlCodeTests.registerAffixDesc)
			r0 virtual(target:enum(cases.EKind))
			r1 enum(cases.EKind)
			.30    @0 new 0
			.30    @1 global 1, 18
			.30    @2 setfield 0[0],1
			.30    @3 ret 0
	</>)
	static public function registerAffixDesc():ApplicationDesc {
		return {target: Empty};
	}
}
