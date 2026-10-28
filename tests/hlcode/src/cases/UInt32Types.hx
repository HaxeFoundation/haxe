package cases;

import haxe.UInt32;

/**
	Tests that document the HL bytecode generated for UInt32 operations.

	UInt32 is backed by Int32Native. Operations that differ for unsigned
	semantics (comparison, division, modulo) use Haxe-level helper functions
	rather than native HL unsigned opcodes (juge/udiv/umod).
	The right-shift operator does use the native `ushr` opcode because
	`UInt32.shr` delegates to `Int32Native.ushr` which uses `a >>> b`.
**/
@:keep
class UInt32Types {
	static final u32a:UInt32 = cast 0;
	static final u32b:UInt32 = cast 0;

	@:pure(false)
	static function use<T>(v:T) {}

	/**
		UInt32 > UInt32 comparison uses Haxe-level ucompare, not a native
		unsigned jump opcode. The result is compared signed (jsgte) against 0.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.UInt32Types.cmpGt)
		r0 void
		r1 bool
		r2 i32
		r3 cases.$UInt32Types
		r4 i32
		r5 dyn
		@0 global 3, $0
		@1 field 2,3[5]
		@2 global 3, $0
		@3 field 4,3[6]
		@4 call 2, haxe.numeric._Int32Direct.Int32Direct_Impl_.ucompare(2,4)
		@5 int 4,@$1
		@6 jsgte 4,2,2
		@7 true 1
		@8 jalways 1
		@9 false 1
		@A todyn 5,1
		@B call 0, cases.UInt32Types.use(5)
		@C ret 0
		</>)
	static function cmpGt() {
		use(u32a > u32b);
	}

	/**
		UInt32 >> Int uses the native `ushr` opcode (unsigned right shift).
		This works because `UInt32.shr` calls `Int32Native.ushr` which uses
		the unsigned shift operator `>>>` on Int.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.UInt32Types.shrOp)
		r0 void
		r1 i32
		r2 cases.$UInt32Types
		r3 i32
		r4 null(i32)
		@0 global 2, $0
		@1 field 1,2[5]
		@2 int 3,@$1
		@3 ushr 1,1,3
		@4 todyn 4,1
		@5 call 0, cases.UInt32Types.use(4)
		@6 ret 0
		</>)
	static function shrOp() {
		use(u32a >> 1);
	}
}
