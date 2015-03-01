
var b = new haxe.io.UInt8Array(5);

// set
for( i in 0...5 )
	b[i] = i + 1;

var buf = b.view.buffer;

buf.get(0) == 1;
buf.get(4) == 5;

var s = b.view.sub(2);
s.byteOffset == 2;
s.byteLength == 3;
s.buffer == buf;

var b2 = haxe.io.UInt8Array.fromBytes(s.buffer, s.byteOffset, s.byteLength);
b2[0] == 3;
b2[0] = 4;
b[3] == 4;
