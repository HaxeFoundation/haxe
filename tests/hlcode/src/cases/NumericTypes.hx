package cases;

import haxe.Int64;
import haxe.Int32;

/**
	Tests that verify correct HL code generation for numeric type operations.
	These ensure that abstract type layering (Int32 → Int32Native, Int64 → Int64Native)
	does not introduce unnecessary intermediate variables or instructions.
**/
@:keep
class NumericTypes {
	static final i32:Int32 = 0;
	static final i64:Int64 = Int64.make(0, 0);

	@:pure(false)
	static function use<T>(v:T) {}

	/**
		Int64 == Int32 comparison should produce a direct toint + jnoteq,
		without any intermediate variable from abstract constructor inlining.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericTypes.eqI64I32)
		r0 i32
		r1 cases.$NumericTypes
		r2 void
		r3 bool
		r4 i64
		r5 i64
		r6 dyn
		@0 global 1, $0
		@1 field 0,1[5]
		@2 global 1, $0
		@3 field 4,1[6]
		@4 toint 5,0
		@5 jnoteq 4,5,2
		@6 true 3
		@7 jalways 1
		@8 false 3
		@9 todyn 6,3
		@A call 2, cases.NumericTypes.use(6)
		@B ret 2
	</>)
	static function eqI64I32() {
		use(i64 == i32);
	}

	/**
		Int64 + Int should produce a direct toint + add,
		without unnecessary temporaries.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericTypes.addI64Int)
		r0 void
		r1 i64
		r2 cases.$NumericTypes
		r3 i32
		r4 i64
		r5 null(i64)
		@0 global 2, $0
		@1 field 1,2[6]
		@2 int 3,@$1
		@3 toint 4,3
		@4 add 1,1,4
		@5 todyn 5,1
		@6 call 0, cases.NumericTypes.use(5)
		@7 ret 0
	</>)
	static function addI64Int() {
		use(i64 + 5);
	}

	/**
		Assigning Int32 to Int64 should produce a single toint,
		not a block with intermediate variable.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericTypes.i32ToI64)
		r0 i32
		r1 cases.$NumericTypes
		r2 void
		r3 i64
		r4 null(i64)
		@0 global 1, $0
		@1 field 0,1[5]
		@2 toint 3,0
		@3 todyn 4,3
		@4 call 2, cases.NumericTypes.use(4)
		@5 ret 2
	</>)
	static function i32ToI64() {
		var x:Int64 = i32;
		use(x);
	}
}
