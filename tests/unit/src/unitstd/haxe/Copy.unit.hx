// Array

var a = [1, 2];
var b = haxe.Copy.copy(a);
1 == b[0];
2 == b[1];
a != b;
var c = [a, a];
var d = haxe.Copy.copy(c);
d[0] != a;
d[1] != a;
d[0] == d[1];
// Anon

var a = {f1: 1, f2: 2};
var b = haxe.Copy.copy(a);
1 == b.f1;
2 == b.f2;
a != b;
var c = {f1: a, f2: a};
var d = haxe.Copy.copy(c);
d.f1 != a;
d.f2 != a;
d.f1 == d.f2;
// Enum

var a = (macro 1);
var b = haxe.Copy.copy(a);
switch [a.expr, b.expr] {
	case [EConst(CInt(a)), EConst(CInt(b))]:
		eq(a, b);
	case _:
		utest.Assert.fail('match failure: ${a.expr} ${b.expr}');
}
