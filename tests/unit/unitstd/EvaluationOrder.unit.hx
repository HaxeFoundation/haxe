function func(i1:Dynamic, i2:Dynamic, i3:Dynamic) {
	return '$i1;$i2;$i3';
}

var i = 0;
func(i++, i++, i++) == "0;1;2";

var a = [i++, i++, i++];
a.join(";") == "3;4;5";

var obj = {
	a: i++,
	b: i++,
	c: i++
}

obj.a = 6;
obj.b = 7;
obj.c = 8;

func(i++, [i++, i++].join(";"), i++) == "9;10;11;12";

var buf:Array<Int> = [];

function a() {
	buf.push(1);
	return 1;
}

function b() {
	buf.push(2);
	return 2;
}

function c() {
	buf.push(3);
	return 3;
}

function d() {
	buf.push(4);
	return 4;
}

function e() {
	buf.push(5);
	return 5;
}

function f() {
	buf.push(6);
	return 6;
}

function begin() {
	buf = [];
	return function() {
		return buf.join("_");
	}
}

// &&

var end = begin();
(a() + b()) >= 0 && (c() + d()) >= 0;
end() == "1_2_3_4";

var end = begin();
(a() + b()) >= 99 && (c() + d()) >= 0;
end() == "1_2";

var end = begin();
(a() + b()) >= 0 && (c() + d()) >= 0 && (e() + f()) >= 0;
end() == "1_2_3_4_5_6";

var end = begin();
(a() + b()) >= 99 && (c() + d()) >= 0 && (e() + f()) >= 0;
end() == "1_2";

var end = begin();
(a() + b()) >= 0 && (c() + d()) >= 99 && (e() + f()) >= 0;
end() == "1_2_3_4";

// ||

var end = begin();
(a() + b()) >= 0 || (c() + d()) >= 0;
end() == "1_2";

var end = begin();
(a() + b()) >= 99 || (c() + d()) >= 0;
end() == "1_2_3_4";

var end = begin();
(a() + b()) >= 0 || (c() + d()) >= 0 || (e() + f()) >= 0;
end() == "1_2";

var end = begin();
(a() + b()) >= 99 || (c() + d()) >= 0 || (e() + f()) >= 0;
end() == "1_2_3_4";

var end = begin();
(a() + b()) >= 99 || (c() + d()) >= 99 || (e() + f()) >= 0;
eq(end(), "1_2_3_4_5_6");

// []

function arr(x, y) {
	return [];
}

function idx(x, y) {
	return 0;
}

var end = begin();
var _ = (arr(a(), b()))[idx(c(), d())];
eq(end(), "1_2_3_4");