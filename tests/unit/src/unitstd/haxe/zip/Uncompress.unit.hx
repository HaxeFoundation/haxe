// not supported in js/python yet
#if (cpp || php || java || neko || flash)
var d = [120, 218, 43, 73, 45, 46, 1, 0, 4, 93, 1, 193];
var b = haxe.io.Bytes.alloc(d.length);
for (i in 0...d.length) b.set(i, d[i]);
var c = haxe.zip.Uncompress.run(b);

c.toString() == "test";

var d = [120, 218, 3, 0, 0, 0, 0, 1];
var b = haxe.io.Bytes.alloc(d.length);
for (i in 0...d.length) b.set(i, d[i]);
var c = haxe.zip.Uncompress.run(b);

c.length == 0;
#end

#if php
var d = [120, 218, 43, 73, 45, 46, 1, 0, 4, 93, 1, 193];
var b = haxe.io.Bytes.alloc(d.length);
for (i in 0...d.length) b.set(i, d[i]);
var c = haxe.io.Bytes.alloc(4);
var d = new haxe.zip.Uncompress();
var r = d.execute(b, 0, c, 0);

r.done == true;
r.read == 12;
r.write == 4;
c.toString() == "test";
#else
1 == 1;
#end
