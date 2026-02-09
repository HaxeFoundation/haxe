package unit.issues;

class Issue11013 extends Test {
	function test() {
		#if lua
		// Test that haxe.Int32 properly wraps on overflow instead of clamping.
		// Before fix, Lua 5.3+ without bit32 would clamp to max/min Int32.
		var max:haxe.Int32 = 2147483647;
		var one:haxe.Int32 = 1;
		eq((max + one : Int), -2147483648); // should wrap around

		var min:haxe.Int32 = -2147483648;
		eq((min - one : Int), 2147483647); // should wrap around

		// Multiplication overflow
		var large:haxe.Int32 = 0x10000;
		eq((large * large : Int), 0); // 2^32 wraps to 0
		#else
		noAssert();
		#end
	}
}
