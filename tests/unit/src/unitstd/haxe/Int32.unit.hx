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
