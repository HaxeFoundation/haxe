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

		o.bigEndian = endian;
		eq(o.bigEndian,endian);

		o.prepare(4);
		o.writeByte(0x00);
		o.writeByte(0x01);
		o.writeByte(0x02);
		o.writeByte(0x03);

		o.write(b);
		o.writeByte(55);
		o.writeBytes(b,3,5);
		excv(function() o.writeBytes(b,-1,5),OutsideBounds);
		excv(function() o.writeBytes(b,3,-1),OutsideBounds);
		excv(function() o.writeBytes(b,3,20),OutsideBounds);

		o.writeByte(98);
		#if (neko || flash9 || php || cpp)
		o.writeDouble(1.23);
		o.writeFloat(1.2e10);
		#end
		o.writeByte(99);

		var str = "Héllo World !";
		o.writeString(str);

		o.writeInt16(-12345);
		excv(function() o.writeInt16(1 << 15),Overflow);
		excv(function() o.writeInt16(-((1 << 15)+1)),Overflow);
		o.writeInt24(-1234567);
		excv(function() o.writeInt16(1 << 24),Overflow);
		excv(function() o.writeInt16(-((1 << 24)+1)),Overflow);
		o.writeInt31(-123456789);
		#if !neko
		// in neko, we can't represent invalid 31 bits integers anyway
		excv(function() o.writeInt31(1 << 30),Overflow);
		excv(function() o.writeInt31(-((1 << 30) + 1)),Overflow);
		#end
		o.writeInt8(-5);
		excv(function() o.writeInt8(128),Overflow);
		excv(function() o.writeInt8(-129),Overflow);
		o.writeUInt16(0xFF55);
		excv(function() o.writeUInt16(1 << 16),Overflow);
		excv(function() o.writeUInt16(-1),Overflow);
		o.writeUInt24(0xFF00EE);
		excv(function() o.writeUInt24(1 << 24),Overflow);
		excv(function() o.writeUInt24(-1),Overflow);
		o.writeUInt30(0x3FAABBCC);
		excv(function() o.writeUInt30(-1),Overflow);
		excv(function() o.writeUInt30(0x40 << 24),Overflow);

		o.writeInt32(haxe.Int32.make(0xA0FF,0xEEDD));
		o.writeInt32(haxe.Int32.make(0xC0FF,0xEEDD));

		unspec(function() o.writeByte(-1));
		unspec(function() o.writeByte(257));

		var i = new haxe.io.BytesInput(o.getBytes());
		i.bigEndian = endian;
		eq( i.readUInt30(), endian ? 0x00010203 : 0x03020100 );
		eq( i.read(b.length).compare(b) , 0 );
		eq( i.readByte(), 55 );
		eq( i.read(5).compare(b.sub(3,5)), 0 );

		eq( i.readByte(), 98 );
		#if (neko || flash9 || php || cpp)
		eq( i.readDouble(), 1.23 );
		eq( i.readFloat(), 1.2e10 );
		#else
		// these two are not implemented
		exc(function() i.readDouble());
		exc(function() i.readFloat());
		#end
		eq( i.readByte(), 99 );

		eq( i.readString(haxe.io.Bytes.ofString(str).length), str );

		eq( i.readInt16(), -12345 );
		eq( i.readInt24(), -1234567 );
		eq( i.readInt31(), -123456789 );
		eq( i.readInt8(), -5 );
		eq( i.readUInt16(), 0xFF55 );
		eq( i.readUInt24(), 0xFF00EE );
		eq( i.readUInt30(), 0x3FAABBCC );

		eq( haxe.Int32.compare( i.readInt32() , haxe.Int32.make(0xA0FF,0xEEDD) ), 0 );
		eq( haxe.Int32.compare( i.readInt32() , haxe.Int32.make(0xC0FF,0xEEDD) ), 0 );

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

}
