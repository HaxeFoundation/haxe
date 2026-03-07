/**
	Tests for HL bytecode output.

	Each test function annotated with `@:hl(<> ... </>)` is checked against
	the generated HL dump. The expected output is copied from `dump/hlcode.txt`
	(compile with `-D dump` to generate it).

	The comparison is normalized: source line numbers and unstable global IDs
	are ignored, making tests robust to unrelated changes.
**/

// --- Types used in tests ---

enum EKind {
	Empty;
}

typedef ApplicationDesc = {
	@:optional final target:EKind;
}

// --- Test class ---

class HlCodeTests {
	static function main():Void {}

	/**
		Test that an anonymous object with an optional enum field is initialized correctly.
		The expected HL output verifies the struct allocation and field assignment.
	**/
	@:hl(<>
		fun@23(17h) ():virtual(target:enum(EKind))
		; src/HlCodeTests.hx:36 (HlCodeTests.registerAffixDesc)
			r0 virtual(target:enum(EKind))
			r1 enum(EKind)
			.36    @0 new 0
			.36    @1 global 1, 7
			.36    @2 setfield 0[0],1
			.36    @3 ret 0
	</>)
	static public function registerAffixDesc():ApplicationDesc {
		return {target: Empty};
	}
}
