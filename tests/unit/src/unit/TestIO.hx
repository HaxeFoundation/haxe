package unit;
import haxe.io.Error;

class TestIO extends Test {

	public function test() {
		check(false);
		check(true);
	}

	function excv<T>( f, e : T, ?pos ) {
		try {
			f();
			eq(null,e,pos);
		} catch( e2 : Dynamic ) {
			eq(e2,e,pos);
		}
	}

	function check(endian:Bool) {
		infos("endian = "+endian);

		var b = haxe.io.Bytes.ofString("ABCééé\r\n\t");
		eq( b.length, 12 );
		b.set(1,0);

		var o = new haxe.io.BytesOutput();
		eq(o.length, 0);

		o.bigEndian = endian;
		eq(o.bigEndian,endian);

		o.prepare(4);
		o.writeByte(0x00);
		o.writeByte(0x01);
		o.writeByte(0x02);
		o.writeByte(0x03);
		eq(o.length, 4);

		o.write(b);
		o.writeByte(55);
		o.writeBytes(b,3,5);
		excv(function() o.writeBytes(b,-1,5),OutsideBounds);
		excv(function() o.writeBytes(b,3,-1),OutsideBounds);
		excv(function() o.writeBytes(b,3,20),OutsideBounds);

		o.writeByte(98);
		o.writeDouble(1.23);
		o.writeDouble(-1.23);
		o.writeDouble(0.0);
		o.writeDouble(-0.0);
		o.writeFloat(1.2e10);
		o.writeFloat(-1.2e10);
		o.writeFloat(0.0);
		o.writeFloat(-0.0);
		o.writeByte(99);

		var str = "Héllo World !";
		o.writeString(str);

		eq(o.length, 86);

		o.writeInt16(-12345);
		excv(function() o.writeInt16(1 << 15),Overflow);
		excv(function() o.writeInt16(-((1 << 15)+1)),Overflow);
		o.writeInt24(-1234567);
		excv(function() o.writeInt16(1 << 24),Overflow);
		excv(function() o.writeInt16(-((1 << 24)+1)),Overflow);
		o.writeInt32(-123456789);

		o.writeInt8(-5);
		excv(function() o.writeInt8(128),Overflow);
		excv(function() o.writeInt8(-129),Overflow);
		o.writeUInt16(0xFF55);
		excv(function() o.writeUInt16(1 << 16),Overflow);
		excv(function() o.writeUInt16(-1),Overflow);
		o.writeUInt24(0xFF00EE);
		excv(function() o.writeUInt24(1 << 24),Overflow);
		excv(function() o.writeUInt24(-1),Overflow);
		o.writeInt32(0x3FAABBCC);

		o.writeInt32(0xA0FFEEDD);
		o.writeInt32(0xC0FFEEDD);

		var i = new haxe.io.BytesInput(o.getBytes());
		i.bigEndian = endian;
		eq( i.position, 0 );
		eq( i.length, 113 );

		eq( i.readInt32(), endian ? 0x00010203 : 0x03020100 );
		eq( i.read(b.length).compare(b) , 0 );
		eq( i.readByte(), 55 );
		eq( i.read(5).compare(b.sub(3,5)), 0 );

		eq( i.readByte(), 98 );
		eq( i.readDouble(), 1.23 );
		eq( i.readDouble(), -1.23 );
		eq( i.readDouble(), 0.0 );
		eq( i.readDouble(), -0.0 );
		eq( i.readFloat(), 1.2e10 );
		eq( i.readFloat(), -1.2e10 );
		eq( i.readFloat(), 0.0 );
		eq( i.readFloat(), -0.0 );
		eq( i.readByte(), 99 );

		eq( i.readString(haxe.io.Bytes.ofString(str).length), str );

		eq( i.readInt16(), -12345 );
		eq( i.readInt24(), -1234567 );
		eq( i.readInt32(), -123456789 );
		eq( i.readInt8(), -5 );
		eq( i.readUInt16(), 0xFF55 );
		eq( i.readUInt24(), 0xFF00EE );
		eq( i.readInt32(), 0x3FAABBCC );

		eq( i.readInt32() , 0xA0FFEEDD );
		eq( i.readInt32() , 0xC0FFEEDD );

		eq( i.position, i.length );
	}

	function testBytesBounds() {
		var b = haxe.io.Bytes.ofString("ABCDEFGHIJ");
		var tmp = haxe.io.Bytes.alloc(7);
		var i = new haxe.io.BytesInput(b);
		excv( function() i.readBytes(tmp,1,7), OutsideBounds );
		excv( function() i.readBytes(tmp,-1,7), OutsideBounds );
		excv( function() i.readBytes(tmp,8,1), OutsideBounds );
		eq( i.readBytes(tmp,0,7), 7 );
		eq( tmp.get(0), "A".code );
		eq( tmp.get(6), "G".code );
		eq( i.readBytes(tmp,0,7), 3 );
		eq( tmp.get(0), "H".code );
		eq( tmp.get(2), "J".code );
		eq( tmp.get(3), "D".code );
		exc( function() i.readBytes(tmp,0,7) );
	}

	function testBytesInputSeek() {
		var b = haxe.io.Bytes.ofString("0123456789abcdef");
		var i = new haxe.io.BytesInput( b );
		i.position = 15;
		eq( i.readByte(), "f".code );
		exc( i.readByte );
		i.position = 1;
		eq( i.readByte(), "1".code );
		eq( i.readByte(), "2".code );
		var tmp = haxe.io.Bytes.alloc(14);
		eq( i.readBytes(tmp,0,13), 13 );
		exc( i.readByte );
		i.position = -10;
		eq( i.position, 0 );
		eq( i.readByte(), "0".code );
		i.position = 999;
		eq( i.position, i.length );
		exc( i.readByte );
	}
}
