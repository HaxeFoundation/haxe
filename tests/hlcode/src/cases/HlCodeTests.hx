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

private interface IDrawable {
	public function render( engine : Engine ) : Void;
}

private class App implements IDrawable {
	public function new() {}
	public function render( e : Engine ) {}
}

private class Engine {
	public function new() {}
	public function render( obj : { function render( engine : Engine ) : Void; } ) {}
}

private typedef MiniRef<T> = {
	public function get():T;
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
			r0 virtual(node:...)
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

	// Passing a class instance where a structural `{ render : ... }` is expected
	// goes through the same virtual as a plain anonymous structure.
	@:hl(<>
		fun@372(174h) ():void
		; src/cases/HlCodeTests.hx:114 (cases.HlCodeTests.testInterfaceToStructural)
			r0 void
			r1 cases._HlCodeTests.Engine
			r2 cases._HlCodeTests.App
			r3 virtual(render:method:(cases._HlCodeTests.Engine):void)
			.114   @0 new 1
			.114   @1 call 0, cases._HlCodeTests.Engine.new(1)
			.114   @2 new 2
			.114   @3 call 0, cases._HlCodeTests.App.new(2)
			.114   @4 jnotnull 2,2
			.114   @5 null 3
			.114   @6 jalways 4
			.114   @7 field 3,2[0]
			.114   @8 jnotnull 3,2
			.114   @9 tovirtual 3,2
			.114   @A setfield 2[0],3
			.114   @B call 0, cases._HlCodeTests.Engine.render(1,3)
			.114   @C ret 0
	</>)
	static public function testInterfaceToStructural() {
		new Engine().render(new App());
	}

	// An object literal whose field holds a function (`get` is a Var) used where
	// a method field is expected (`MiniRef.get` is a Method) must NOT be conflated
	// with the method-bearing virtual: the field stays a plain function value.
	@:hl(<>
		fun@373(175h) ():virtual(get:method:():i32)
		; src/cases/HlCodeTests.hx:121 (cases.HlCodeTests.testVarFieldVsMethod)
			r0 dynobj
			r1 ():i32
			r2 virtual(get:method:():i32)
			.121   @0 new 0
			.121   @1 staticclosure 1, fun$374
			.121   @2 dynset 0[@174],1
			.121   @3 tovirtual 2,0
			.121   @4 ret 2
	</>)
	static public function testVarFieldVsMethod():MiniRef<Int> {
		return { get: function() return 1 };
	}
}
