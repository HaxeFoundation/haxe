
var out = new haxe.io.BytesBuffer();

out.length == 0;
out.add( haxe.io.Bytes.ofString("ABCDEF") );

out.length == 6;

for( i in 1...6 )
	out.addByte(i);

out.addBytes( haxe.io.Bytes.ofString("ABCDEF"),1,3 );

out.length == 14;

var b = out.getBytes();
var str = "ABCDEF\x01\x02\x03\x04\x05BCD";
b.length == str.length;
for( i in 0...str.length )
	b.get(i) == str.charCodeAt(i);

var out = new haxe.io.BytesBuffer();
out.addInt32(0xABCDEF00);
out.addByte(42);
out.addFloat(1.3);
out.addDouble(2.4);
out.addInt64(haxe.Int64.make(0xABCDEF00,0xCAFFEED1));

var b = out.getBytes();

b.length == 25;

b.getInt32(0) == 0xABCDEF00;
b.get(4) == 42;
b.getFloat(5) == 1.2999999523162842;
b.getDouble(9) == 2.4;
t(b.getInt64(17) == haxe.Int64.make(0xABCDEF00,0xCAFFEED1));

// check correct low endian encoding
b.get(3) == 0xAB;
b.get(5) == 102;
b.get(9) == 51;
b.get(17) == 0xD1;
b.get(22) == 0xEF;
