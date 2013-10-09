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