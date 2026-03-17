package unit.teststd.haxe;

class TestInt32 extends unit.Test {
	public function test() {
		// test op overflows
		var max:haxe.Int32 = 0x7fffffff;
		var min:haxe.Int32 = 0x80000000;

		var a:haxe.Int32 = 0x7fffffff;
		eq(a++, max);
		eq(a, min);
		eq(a--, min);
		eq(a, max);
		eq(++a, min);
		eq(--a, max);

		eq(max+min, -1);
		eq(max+1, min);

		eq(max-min, -1);
		eq(min-1, max);

		eq(max*max, 1);
		eq(max*min, -2147483648);
		eq(max*2, -2);

		eq(min << 1, 0);
		eq(min >> 1, 0xc0000000);
		eq(min >>> 1, 0x40000000);

		#if !cpp
		var a = [1];
		var next = 0;

		var i32:haxe.Int32 = max - 1;
		i32 |= ((a[next] << 32) | 1 );
		eq(i32, max);

		var i32:haxe.Int32 = ((a[next] << 33) | 3);
		i32 >>= 1;
		eq(i32, 1);

		var i32:haxe.Int32 = 2;
		i32 ^= ( (a[next] << 32) | 1);
		eq(i32, 3);

		var i32:haxe.Int32 = 2;
		var c = ~(((a[next] << 32) | 1):haxe.Int32);
		eq(c, 0xfffffffe);
		#end

		// - see: https://github.com/HaxeFoundation/haxe/pull/7491
		-min == min;              // two's complement overflow,
		-2147483643 == 5 + -min;  // order of ops and negate
		2147483643 == -(5 + min); // static analyzer issue

		#if hl
		0 == min % 0;              // % 0 div by zero exception
		eq(0, Std.int(min / 0));
		0 == min % -1;             // min % -1 integer overflow exception
		eq(min, Std.int(min / -1));
		eq(min, min * -1);
		eq(0, min % 1);
		eq(0, max % 0);
		eq(0, Std.int(max / 0));
		eq(0, max % -1);
		eq(-max, Std.int(max / -1));
		eq(-max, max * -1);
		eq(0, max % 1);
		#end

	}
}
