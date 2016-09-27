// not supported in js/python/cs yet
#if (cpp || php || java || neko || flash)
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
