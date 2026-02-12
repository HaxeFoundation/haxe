package unit.issues;

class Issue9475 extends Test {
	function test() {
		#if lua
		// Test that Int32 bitwise operations work without bit/bit32 libraries
		// (native Lua 5.3+ operators should be used as fallback)
		var a:Int = 0xFF00;
		var b:Int = 0x0FF0;

		// AND
		eq(a & b, 0x0F00);

		// OR
		eq(a | b, 0xFFF0);

		// XOR
		eq(a ^ b, 0xF0F0);

		// Left shift
		eq(1 << 8, 256);

		// Arithmetic right shift
		eq(256 >> 4, 16);

		// Unsigned right shift
		eq(256 >>> 4, 16);

		// Complement (unary NOT)
		eq(~0, -1);
		eq(~1, -2);

		// Verify Int32 wrapping with shifts
		eq(1 << 31, -2147483648);
		eq(-2147483648 >> 31, -1);
		#else
		noAssert();
		#end
	}
}
