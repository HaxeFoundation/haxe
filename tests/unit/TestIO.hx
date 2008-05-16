package unit;

class TestIO extends Test {

	public function test() {
		check(false);
		check(true);
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
		exc(function() o.writeBytes(b,-1,5));
		exc(function() o.writeBytes(b,3,-1));
		exc(function() o.writeBytes(b,3,20));

		o.writeByte(98);
		#if (neko || flash9)
		o.writeDouble(1.23);
		o.writeFloat(1.2e10);
		#end
		o.writeByte(99);

		o.writeInt16(-12345);
		exc(function() o.writeInt16(1 << 15));
		exc(function() o.writeInt16(-((1 << 15)+1)));
		o.writeInt24(-1234567);
		exc(function() o.writeInt16(1 << 24));
		exc(function() o.writeInt16(-((1 << 24)+1)));
		o.writeInt31(-123456789);
		#if neko
		// in neko, we can't represent invalid 31 bits integers anyway
		exc(function() throw "Overflow");
		exc(function() throw "Overflow");
		#else
		exc(function() o.writeInt31(1 << 30));
		exc(function() o.writeInt31(-((1 << 30) + 1)));
		#end
		o.writeInt8(-5);
		exc(function() o.writeInt8(128));
		exc(function() o.writeInt8(-129));
		o.writeUInt16(0xFF55);
		exc(function() o.writeUInt16(1 << 16));
		exc(function() o.writeUInt16(-1));
		o.writeUInt24(0xFF00EE);
		exc(function() o.writeUInt24(1 << 24));
		exc(function() o.writeUInt24(-1));
		o.writeUInt30(0x3FAABBCC);
		exc(function() o.writeUInt30(-1));
		exc(function() o.writeUInt30(0x40 << 24));

		unspec(function() o.writeByte(-1));
		unspec(function() o.writeByte(257));

		var i = new haxe.io.BytesInput(o.getBytes());
		i.bigEndian = endian;
		eq( i.readUInt30(), endian ? 0x00010203 : 0x03020100 );
		eq( i.read(b.length).compare(b) , 0 );
		eq( i.readByte(), 55 );
		eq( i.read(5).compare(b.sub(3,5)), 0 );

		eq( i.readByte(), 98 );
		#if (neko || flash9)
		eq( i.readDouble(), 1.23 );
		eq( i.readFloat(), 1.2e10 );
		#else
		// these two are not implemented
		exc(function() i.readDouble());
		exc(function() i.readFloat());
		#end
		eq( i.readByte(), 99 );

		eq( i.readInt16(), -12345 );
		eq( i.readInt24(), -1234567 );
		eq( i.readInt31(), -123456789 );
		eq( i.readInt8(), -5 );
		eq( i.readUInt16(), 0xFF55 );
		eq( i.readUInt24(), 0xFF00EE );
		eq( i.readUInt30(), 0x3FAABBCC );
	}

}