package cases;

import haxe.Int32;
import haxe.UInt32;
import haxe.Int64;
import haxe.UInt64;

/**
	Tests documenting HL bytecode for all implicit and explicit type conversions
	between (U)Int32/64 and `Int`, plus `==`, `<`, `+`, `<<` for each combination.

	Key observations:
	- Int32, UInt32 and Int all map to `i32` in HL, so conversions between them
	  are zero-cost (just a field read; no cast instruction).
	- Int64 and UInt64 share the same `i64` backing, so Int64 ↔ UInt64 is also
	  zero-cost.
	- Int → Int64/UInt64 uses `toint` (sign-extends the 32-bit value to 64 bits).
	- Int64/UInt64 → Int uses `toint` (truncates to the low 32 bits).
	- UInt32 → Int64/UInt64 zero-extension cannot use a native HL opcode and is
	  therefore emitted as a mask-and-or sequence (field read for the mask constant
	  from Int64NativeImpl, AND with the low 32 bits, OR with a zero high word).
	- Int32 < Int dispatches to `Int32Native.lt`, producing a direct `jsgte`
	  instruction (the HL inverse of `<`).  Int64 < Int similarly uses
	  `Int64Native.lt`, producing a direct `jsgte i64`.
	  UInt32 and UInt64 comparisons still use the `ucompare` helper because
	  unsigned comparison requires sign-handling logic.
	  UInt64 < Int expands to the full 64-bit `ucompare` logic.
**/
@:keep
class NumericConversions {
	static final i:Int = 0;
	static final i32:Int32 = 0;
	static final u32:UInt32 = cast 0;
	static final i64:Int64 = Int64.make(0, 0);
	static final u64:UInt64 = cast Int64.make(0, 0);

	@:pure(false)
	static function use<T>(v:T) {}

	// ─── Explicit narrowing: (U)Int64 → Int (truncate to low 32 bits) ─────────

	/**
		Int64.toInt() uses `toint` to truncate the i64 to i32.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.i64ToInt)
		r0 i64
		r1 cases.$NumericConversions
		r2 i32
		r3 void
		r4 null(i32)
		@0 global 1, $0
		@1 field 0,1[8]
		@2 toint 2,0
		@3 todyn 4,2
		@4 call 3, cases.NumericConversions.use(4)
		@5 ret 3
	</>)
	static function i64ToInt() {
		var x:Int = Int64.toInt(i64);
		use(x);
	}

	/**
		UInt64.toInt() also uses `toint` (truncates to low 32 bits), identical to
		Int64.toInt() at the HL level.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.u64ToInt)
		r0 i64
		r1 cases.$NumericConversions
		r2 i32
		r3 void
		r4 null(i32)
		@0 global 1, $0
		@1 field 0,1[9]
		@2 toint 2,0
		@3 todyn 4,2
		@4 call 3, cases.NumericConversions.use(4)
		@5 ret 3
	</>)
	static function u64ToInt() {
		var x:Int = UInt64.toInt(u64);
		use(x);
	}

	// ─── Explicit: Int32/UInt32 → Int (zero-cost: both are i32 in HL) ─────────

	/**
		Int32 → Int via the @:to toInt() method.
		Both types are backed by i32 in HL so no cast instruction is emitted.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.i32ToInt)
		r0 i32
		r1 cases.$NumericConversions
		r2 void
		r3 null(i32)
		@0 global 1, $0
		@1 field 0,1[6]
		@2 todyn 3,0
		@3 call 2, cases.NumericConversions.use(3)
		@4 ret 2
	</>)
	static function i32ToInt() {
		var x:Int = i32.toInt();
		use(x);
	}

	/**
		UInt32 → Int via the toInt() instance method.
		Also zero-cost: both types are backed by i32.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.u32ToInt)
		r0 i32
		r1 cases.$NumericConversions
		r2 void
		r3 null(i32)
		@0 global 1, $0
		@1 field 0,1[7]
		@2 todyn 3,0
		@3 call 2, cases.NumericConversions.use(3)
		@4 ret 2
	</>)
	static function u32ToInt() {
		var x:Int = u32.toInt();
		use(x);
	}

	// ─── Explicit: Int → (U)Int32 (zero-cost) ─────────────────────────────────

	/**
		Int → Int32 via Int32.fromInt().
		Zero-cost: both are i32 in HL, so the `i` field is read directly.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.intToI32)
		r0 i32
		r1 cases.$NumericConversions
		r2 void
		r3 null(i32)
		@0 global 1, $0
		@1 field 0,1[5]
		@2 todyn 3,0
		@3 call 2, cases.NumericConversions.use(3)
		@4 ret 2
	</>)
	static function intToI32() {
		var x:Int32 = Int32.fromInt(i);
		use(x);
	}

	/**
		Int → UInt32 via UInt32.fromInt(). Identical bytecode to intToI32 since
		both types are i32 in HL.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.intToU32)
		r0 i32
		r1 cases.$NumericConversions
		r2 void
		r3 null(i32)
		@0 global 1, $0
		@1 field 0,1[5]
		@2 todyn 3,0
		@3 call 2, cases.NumericConversions.use(3)
		@4 ret 2
	</>)
	static function intToU32() {
		var x:UInt32 = UInt32.fromInt(i);
		use(x);
	}

	// ─── Explicit: Int → Int64/UInt64 (sign-extends via toint) ────────────────

	/**
		Int → Int64 via Int64.fromInt(). Uses `toint` to sign-extend i32 to i64.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.intToI64)
		r0 i32
		r1 cases.$NumericConversions
		r2 i64
		r3 void
		r4 null(i64)
		@0 global 1, $0
		@1 field 0,1[5]
		@2 toint 2,0
		@3 todyn 4,2
		@4 call 3, cases.NumericConversions.use(4)
		@5 ret 3
	</>)
	static function intToI64() {
		var x:Int64 = Int64.fromInt(i);
		use(x);
	}

	/**
		Int → UInt64 via UInt64.fromInt(). Also sign-extends (same bytecode as
		intToI64) — a negative Int becomes a large UInt64 value.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.intToU64)
		r0 i32
		r1 cases.$NumericConversions
		r2 i64
		r3 void
		r4 null(i64)
		@0 global 1, $0
		@1 field 0,1[5]
		@2 toint 2,0
		@3 todyn 4,2
		@4 call 3, cases.NumericConversions.use(4)
		@5 ret 3
	</>)
	static function intToU64() {
		var x:UInt64 = UInt64.fromInt(i);
		use(x);
	}

	// ─── Implicit widening: Int32/UInt32 → Int64/UInt64 ───────────────────────

	/**
		Int32 → Int64 implicit widening via @:from Int32 in Int64.
		Uses `toint` to sign-extend i32 to i64 (same as explicit Int64.fromInt).
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.i32ImplicitToI64)
		r0 i32
		r1 cases.$NumericConversions
		r2 void
		r3 i64
		r4 null(i64)
		@0 global 1, $0
		@1 field 0,1[6]
		@2 toint 3,0
		@3 todyn 4,3
		@4 call 2, cases.NumericConversions.use(4)
		@5 ret 2
	</>)
	static function i32ImplicitToI64() {
		var x:Int64 = i32;
		use(x);
	}

	#if todo
	/**
		UInt32 → Int64 implicit zero-extension via @:from UInt32 in Int64.
		HL has no unsigned-extend opcode, so zero-extension is emitted as:
		mask the i32 to 32 bits (AND with 0xFFFFFFFF from Int64NativeImpl),
		shift the zero high word, and OR together.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.u32ImplicitToI64)
		r0 i32
		r1 cases.$NumericConversions
		r2 void
		r3 i32
		r4 i64
		r5 i64
		r6 i64
		r7 i64
		r8 i64
		r9 haxe.numeric._Int64Native.$Int64NativeImpl_Impl_
		r10 null(i64)
		@0 global 1, $0
		@1 field 0,1[7]
		@2 int 3,@$1
		@3 toint 4,3
		@4 toint 5,0
		@5 int 3,@$2
		@6 toint 7,3
		@7 shl 6,4,7
		@8 global 9, $3
		@9 field 8,9[5]
		@A and 7,5,8
		@B or 6,6,7
		@C todyn 10,6
		@D call 2, cases.NumericConversions.use(10)
		@E ret 2
	</>)
	static function u32ImplicitToI64() {
		var x:Int64 = u32;
		use(x);
	}
	#end

	/**
		UInt32 → UInt64 implicit zero-extension via @:from UInt32 in UInt64.
		Identical bytecode to u32ImplicitToI64 since Int64 and UInt64 share the
		same i64 backing type.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.u32ImplicitToU64)
		r0 i32
		r1 cases.$NumericConversions
		r2 void
		r3 i32
		r4 i64
		r5 i64
		r6 i64
		r7 i64
		r8 i64
		r9 haxe.numeric._Int64Native.$Int64NativeImpl_Impl_
		r10 null(i64)
		@0 global 1, $0
		@1 field 0,1[7]
		@2 int 3,@$1
		@3 toint 4,3
		@4 toint 5,0
		@5 int 3,@$2
		@6 toint 7,3
		@7 shl 6,4,7
		@8 global 9, $3
		@9 field 8,9[5]
		@A and 7,5,8
		@B or 6,6,7
		@C todyn 10,6
		@D call 2, cases.NumericConversions.use(10)
		@E ret 2
	</>)
	static function u32ImplicitToU64() {
		var x:UInt64 = u32;
		use(x);
	}

	/**
		Int64 → UInt64 implicit same-size reinterpret via @:from Int64 in UInt64.
		Zero-cost: both share the same i64 backing type in HL.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.i64ImplicitToU64)
		r0 i64
		r1 cases.$NumericConversions
		r2 void
		r3 null(i64)
		@0 global 1, $0
		@1 field 0,1[8]
		@2 todyn 3,0
		@3 call 2, cases.NumericConversions.use(3)
		@4 ret 2
	</>)
	static function i64ImplicitToU64() {
		var x:UInt64 = i64;
		use(x);
	}

	// ─── Implicit same-size: Int32 ↔ UInt32 ───────────────────────────────────

	/**
		Int32 → UInt32 implicit same-size reinterpret via @:from Int32 in UInt32.
		Zero-cost: both are i32 in HL.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.i32ImplicitToU32)
		r0 i32
		r1 cases.$NumericConversions
		r2 void
		r3 null(i32)
		@0 global 1, $0
		@1 field 0,1[6]
		@2 todyn 3,0
		@3 call 2, cases.NumericConversions.use(3)
		@4 ret 2
	</>)
	static function i32ImplicitToU32() {
		var x:UInt32 = i32;
		use(x);
	}

	/**
		UInt32 → Int32 implicit same-size reinterpret via @:from UInt32 in Int32.
		Zero-cost: both are i32 in HL.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.u32ImplicitToI32)
		r0 i32
		r1 cases.$NumericConversions
		r2 void
		r3 null(i32)
		@0 global 1, $0
		@1 field 0,1[7]
		@2 todyn 3,0
		@3 call 2, cases.NumericConversions.use(3)
		@4 ret 2
	</>)
	static function u32ImplicitToI32() {
		var x:Int32 = u32;
		use(x);
	}

	/**
		Int32 → Int implicit via @:to toInt() in Int32.
		Zero-cost (same as i32ToInt): just reads the i32 field.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.i32ImplicitToInt)
		r0 i32
		r1 cases.$NumericConversions
		r2 void
		r3 null(i32)
		@0 global 1, $0
		@1 field 0,1[6]
		@2 todyn 3,0
		@3 call 2, cases.NumericConversions.use(3)
		@4 ret 2
	</>)
	static function i32ImplicitToInt() {
		var x:Int = i32;
		use(x);
	}

	// ─── Int32 × Int operations ───────────────────────────────────────────────

	/**
		Int32 == Int: via equalsInt<T:Int> @:commutative overload.
		Both are i32 in HL so a direct `jnoteq` is used.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.i32EqInt)
		r0 void
		r1 bool
		r2 i32
		r3 cases.$NumericConversions
		r4 i32
		r5 dyn
		@0 global 3, $0
		@1 field 2,3[6]
		@2 global 3, $0
		@3 field 4,3[5]
		@4 jnoteq 2,4,2
		@5 true 1
		@6 jalways 1
		@7 false 1
		@8 todyn 5,1
		@9 call 0, cases.NumericConversions.use(5)
		@A ret 0
	</>)
	static function i32EqInt() {
		use(i32 == i);
	}

	/**
		Int32 < Int: Int is promoted to Int32 via @:from, then Int32.lt is called.
		Int32.lt dispatches to Int32Native.lt, producing a direct `jsgte`
		(the HL inverse of `<`) instead of the multi-branch compare-then-check sequence.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.i32LtInt)
		r0 void
		r1 bool
		r2 i32
		r3 cases.$NumericConversions
		r4 i32
		r5 dyn
		@0 global 3, $0
		@1 field 2,3[6]
		@2 global 3, $0
		@3 field 4,3[5]
		@4 jsgte 2,4,2
		@5 true 1
		@6 jalways 1
		@7 false 1
		@8 todyn 5,1
		@9 call 0, cases.NumericConversions.use(5)
		@A ret 0
	</>)
	static function i32LtInt() {
		use(i32 < i);
	}

	/**
		Int32 + Int: Int promoted to Int32 via @:from, then Int32 + Int32.
		Results in a direct `add i32, i32` in HL.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.i32AddInt)
		r0 void
		r1 i32
		r2 cases.$NumericConversions
		r3 i32
		r4 null(i32)
		@0 global 2, $0
		@1 field 1,2[6]
		@2 global 2, $0
		@3 field 3,2[5]
		@4 add 1,1,3
		@5 todyn 4,1
		@6 call 0, cases.NumericConversions.use(4)
		@7 ret 0
	</>)
	static function i32AddInt() {
		use(i32 + i);
	}

	/**
		Int32 << Int: direct shl(a:Int32, b:Int) overload; emits native `shl i32`.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.i32ShlInt)
		r0 void
		r1 i32
		r2 cases.$NumericConversions
		r3 i32
		r4 null(i32)
		@0 global 2, $0
		@1 field 1,2[6]
		@2 global 2, $0
		@3 field 3,2[5]
		@4 shl 1,1,3
		@5 todyn 4,1
		@6 call 0, cases.NumericConversions.use(4)
		@7 ret 0
	</>)
	static function i32ShlInt() {
		use(i32 << i);
	}

	// ─── UInt32 × Int operations ──────────────────────────────────────────────

	/**
		UInt32 == Int: via equalsInt<T:Int> @:commutative overload.
		Both are i32 in HL so a direct `jnoteq` is used (bit-pattern equality).
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.u32EqInt)
		r0 i32
		r1 cases.$NumericConversions
		r2 void
		r3 i32
		r4 bool
		r5 dyn
		@0 global 1, $0
		@1 field 0,1[7]
		@2 global 1, $0
		@3 field 3,1[5]
		@4 jnoteq 0,3,2
		@5 true 4
		@6 jalways 1
		@7 false 4
		@8 todyn 5,4
		@9 call 2, cases.NumericConversions.use(5)
		@A ret 2
	</>)
	static function u32EqInt() {
		use(u32 == i);
	}

	/**
		UInt32 < Int: Int is promoted to UInt32 via @:from, then UInt32 < UInt32.
		UInt32 comparison delegates to Int32Direct.ucompare (no native unsigned
		jump opcode is used; result compared signed against 0).
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.u32LtInt)
		r0 void
		r1 bool
		r2 i32
		r3 cases.$NumericConversions
		r4 i32
		r5 dyn
		@0 global 3, $0
		@1 field 2,3[7]
		@2 global 3, $0
		@3 field 4,3[5]
		@4 call 2, haxe.numeric._Int32Direct.Int32Direct_Impl_.ucompare(2,4)
		@5 int 4,@$1
		@6 jsgte 2,4,2
		@7 true 1
		@8 jalways 1
		@9 false 1
		@A todyn 5,1
		@B call 0, cases.NumericConversions.use(5)
		@C ret 0
	</>)
	static function u32LtInt() {
		use(u32 < i);
	}

	/**
		UInt32 + Int: Int promoted to UInt32 via @:from, then UInt32 + UInt32.
		Results in a direct `add i32, i32` in HL.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.u32AddInt)
		r0 void
		r1 i32
		r2 cases.$NumericConversions
		r3 i32
		r4 null(i32)
		@0 global 2, $0
		@1 field 1,2[7]
		@2 global 2, $0
		@3 field 3,2[5]
		@4 add 1,1,3
		@5 todyn 4,1
		@6 call 0, cases.NumericConversions.use(4)
		@7 ret 0
	</>)
	static function u32AddInt() {
		use(u32 + i);
	}

	/**
		UInt32 << Int: direct shl(a:UInt32, b:Int) overload; emits native `shl i32`.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.u32ShlInt)
		r0 void
		r1 i32
		r2 cases.$NumericConversions
		r3 i32
		r4 null(i32)
		@0 global 2, $0
		@1 field 1,2[7]
		@2 global 2, $0
		@3 field 3,2[5]
		@4 shl 1,1,3
		@5 todyn 4,1
		@6 call 0, cases.NumericConversions.use(4)
		@7 ret 0
	</>)
	static function u32ShlInt() {
		use(u32 << i);
	}

	// ─── Int64 × Int operations ───────────────────────────────────────────────

	/**
		Int64 == Int: Int is promoted to Int64 via @:from Int (sign-extends),
		then compared as two i64 values with `jnoteq`.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.i64EqInt)
		r0 void
		r1 bool
		r2 i64
		r3 cases.$NumericConversions
		r4 i32
		r5 i64
		r6 dyn
		@0 global 3, $0
		@1 field 2,3[8]
		@2 global 3, $0
		@3 field 4,3[5]
		@4 toint 5,4
		@5 jnoteq 2,5,2
		@6 true 1
		@7 jalways 1
		@8 false 1
		@9 todyn 6,1
		@A call 0, cases.NumericConversions.use(6)
		@B ret 0
	</>)
	static function i64EqInt() {
		use(i64 == i);
	}

	/**
		Int64 < Int: Int sign-extended to i64 via `toint`, then Int64.lt called.
		Int64.lt dispatches to Int64Native.lt, producing a direct `jsgte` on i64
		instead of the multi-branch compare-then-check sequence.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.i64LtInt)
		r0 void
		r1 bool
		r2 i64
		r3 cases.$NumericConversions
		r4 i32
		r5 i64
		r6 dyn
		@0 global 3, $0
		@1 field 2,3[8]
		@2 global 3, $0
		@3 field 4,3[5]
		@4 toint 5,4
		@5 jsgte 2,5,2
		@6 true 1
		@7 jalways 1
		@8 false 1
		@9 todyn 6,1
		@A call 0, cases.NumericConversions.use(6)
		@B ret 0
	</>)
	static function i64LtInt() {
		use(i64 < i);
	}

	/**
		Int64 + Int: Int sign-extended to i64 via `toint`, then direct `add i64`.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.i64AddInt)
		r0 void
		r1 i64
		r2 cases.$NumericConversions
		r3 i32
		r4 i64
		r5 null(i64)
		@0 global 2, $0
		@1 field 1,2[8]
		@2 global 2, $0
		@3 field 3,2[5]
		@4 toint 4,3
		@5 add 1,1,4
		@6 todyn 5,1
		@7 call 0, cases.NumericConversions.use(5)
		@8 ret 0
	</>)
	static function i64AddInt() {
		use(i64 + i);
	}

	/**
		Int64 << Int: direct shl(a:Int64, b:Int) overload.
		HL `shl` for i64 takes an i64 shift amount, so Int is `toint`-extended first.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.i64ShlInt)
		r0 void
		r1 i64
		r2 cases.$NumericConversions
		r3 i32
		r4 i64
		r5 null(i64)
		@0 global 2, $0
		@1 field 1,2[8]
		@2 global 2, $0
		@3 field 3,2[5]
		@4 toint 4,3
		@5 shl 1,1,4
		@6 todyn 5,1
		@7 call 0, cases.NumericConversions.use(5)
		@8 ret 0
	</>)
	static function i64ShlInt() {
		use(i64 << i);
	}

	// ─── UInt64 × Int operations ──────────────────────────────────────────────

	/**
		UInt64 == Int: via the dedicated eqInt(a:UInt64, b:Int) @:commutative
		overload which sign-extends Int to i64 and compares bit-for-bit.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.u64EqInt)
		r0 void
		r1 bool
		r2 i64
		r3 cases.$NumericConversions
		r4 i32
		r5 i64
		r6 dyn
		@0 global 3, $0
		@1 field 2,3[9]
		@2 global 3, $0
		@3 field 4,3[5]
		@4 toint 5,4
		@5 jnoteq 2,5,2
		@6 true 1
		@7 jalways 1
		@8 false 1
		@9 todyn 6,1
		@A call 0, cases.NumericConversions.use(6)
		@B ret 0
	</>)
	static function u64EqInt() {
		use(u64 == i);
	}

	/**
		UInt64 < Int: Int sign-extended to UInt64 via @:from Int, then UInt64 < UInt64.
		UInt64 comparison expands to the full 64-bit ucompare logic (high-word then
		low-word unsigned branch tree), producing the longest bytecode here.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.u64LtInt)
		r0 i64
		r1 cases.$NumericConversions
		r2 void
		r3 i32
		r4 i64
		r5 bool
		r6 i64
		r7 i32
		r8 dyn
		@0 global 1, $0
		@1 field 0,1[9]
		@2 global 1, $0
		@3 field 3,1[5]
		@4 toint 4,3
		@5 int 3,@$1
		@6 toint 6,3
		@7 jsgte 0,6,13
		@8 int 3,@$1
		@9 toint 6,3
		@A jsgte 4,6,8
		@B jsgte 0,4,2
		@C int 3,@$2
		@D jalways 4
		@E jsgte 4,0,2
		@F int 3,@$3
		@10 jalways 1
		@11 int 3,@$1
		@12 jalways 1
		@13 int 3,@$3
		@14 jalways 12
		@15 int 3,@$1
		@16 toint 6,3
		@17 jsgte 4,6,2
		@18 int 3,@$2
		@19 jalways 7
		@1A jsgte 0,4,2
		@1B int 3,@$2
		@1C jalways 4
		@1D jsgte 4,0,2
		@1E int 3,@$3
		@1F jalways 1
		@20 int 3,@$1
		@21 int 7,@$1
		@22 jsgte 3,7,2
		@23 true 5
		@24 jalways 1
		@25 false 5
		@26 todyn 8,5
		@27 call 2, cases.NumericConversions.use(8)
		@28 ret 2
	</>)
	static function u64LtInt() {
		use(u64 < i);
	}

	/**
		UInt64 + Int: via the dedicated addInt(a:UInt64, b:Int) @:commutative
		overload. Int sign-extended to i64 via `toint`, then direct `add i64`.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.u64AddInt)
		r0 void
		r1 i64
		r2 cases.$NumericConversions
		r3 i32
		r4 i64
		r5 null(i64)
		@0 global 2, $0
		@1 field 1,2[9]
		@2 global 2, $0
		@3 field 3,2[5]
		@4 toint 4,3
		@5 add 1,1,4
		@6 todyn 5,1
		@7 call 0, cases.NumericConversions.use(5)
		@8 ret 0
	</>)
	static function u64AddInt() {
		use(u64 + i);
	}

	/**
		UInt64 << Int: direct shl(a:UInt64, b:Int) overload.
		Like Int64 << Int, the shift amount is `toint`-extended to i64 for HL.
	**/
	@:hl(<>
		fun@N(Nh) ():void
		; (cases.NumericConversions.u64ShlInt)
		r0 void
		r1 i64
		r2 cases.$NumericConversions
		r3 i32
		r4 i64
		r5 null(i64)
		@0 global 2, $0
		@1 field 1,2[9]
		@2 global 2, $0
		@3 field 3,2[5]
		@4 toint 4,3
		@5 shl 1,1,4
		@6 todyn 5,1
		@7 call 0, cases.NumericConversions.use(5)
		@8 ret 0
	</>)
	static function u64ShlInt() {
		use(u64 << i);
	}
}
