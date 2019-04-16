// test op overflows
var max:haxe.Int32 = 0x7fffffff;
var min:haxe.Int32 = 0x80000000;

var a:haxe.Int32 = 0x7fffffff;
a++ == max;
a == min;
a-- == min;
a == max;
++a == min;
--a == max;

max+min == -1;
max+1 == min;

max-min == -1;
min-1 == max;

max*max == 1;
max*min == -2147483648;
max*2 == -2;

min << 1 == 0;
min >> 1 == 0xc0000000;
min >>> 1 == 0x40000000;

#if !cpp
var a = [1];
var next = 0;

var i32:haxe.Int32 = max - 1;
i32 |= ((a[next] << 32) | 1 );
i32 == max;

var i32:haxe.Int32 = ((a[next] << 33) | 3);
i32 >>= 1;
i32 == 1;

var i32:haxe.Int32 = 2;
i32 ^= ( (a[next] << 32) | 1);
i32 == 3;

var i32:haxe.Int32 = 2;
var c = ~(((a[next] << 32) | 1):haxe.Int32);
c == 0xfffffffe;
#end

// - see: https://github.com/HaxeFoundation/haxe/pull/7491
-min == min;              // two's complement overflow,
-2147483643 == 5 + -min;  // order of ops and negate
2147483643 == -(5 + min); // static analyzer issue
