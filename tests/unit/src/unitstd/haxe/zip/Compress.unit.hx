// not supported in js/python yet
#if (cpp || php || java || neko || flash || hl)
var b = haxe.io.Bytes.ofString("test");
var c = haxe.zip.Compress.run(b, 9);

c.length == 12;
c.get(0) == 120;
c.get(1) == 218;
c.get(2) == 43;
c.get(3) == 73;
c.get(4) == 45;
c.get(5) == 46;
c.get(6) == 1;
c.get(7) == 0;
c.get(8) == 4;
c.get(9) == 93;
c.get(10) == 1;
c.get(11) == 193;

var b = haxe.io.Bytes.alloc(0);
var c = haxe.zip.Compress.run(b, 9);

c.length == 8;
c.get(0) == 120;
c.get(1) == 218;
c.get(2) == 3;
c.get(3) == 0;
c.get(4) == 0;
c.get(5) == 0;
c.get(6) == 0;
c.get(7) == 1;
#end

#if php
var b = haxe.io.Bytes.alloc(0);
var c = haxe.io.Bytes.alloc(8);
var d = new haxe.zip.Compress(9);
var r = d.execute(b, 0, c, 0);

r.done == true;
r.read == 0;
r.write == 8;
c.length == 8;
c.get(0) == 120;
c.get(1) == 218;
c.get(2) == 3;
c.get(3) == 0;
c.get(4) == 0;
c.get(5) == 0;
c.get(6) == 0;
c.get(7) == 1;
#else
1 == 1;
#end
