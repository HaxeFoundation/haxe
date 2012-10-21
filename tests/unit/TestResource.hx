package unit;

class TestResource extends Test {

	static var STR = "Héllo World !";

	function testResources() {
		var names = haxe.Resource.listNames();
		eq( names.length, 2 );
		if( names[0] == "res1.txt" )
			eq( names[1], "res2.bin" );
		else {
			eq( names[0], "res2.bin" );
			eq( names[1], "res1.txt" );
		}
		eq( haxe.Resource.getString("res1.txt"), STR );
		#if (neko || php)
		// allow binary strings
		eq( haxe.Resource.getBytes("res2.bin").sub(0,9).toString(), "MZ\x90\x00\x03\x00\x00\x00\x04" );
		#else
		// cut until first \0
		eq( haxe.Resource.getString("res2.bin").substr(0,2), "MZ" );
		#end
		eq( haxe.Resource.getBytes("res1.txt").compare(haxe.io.Bytes.ofString(STR)), 0 );
		var b = haxe.Resource.getBytes("res2.bin");
		var firsts = [0x4D,0x5A,0x90,0x00,0x03,0x00,0x00,0x00,0x04,0x00,0x00,0x00,0xFF,0xFF,0x00,0x00,0xB8];
		var lasts = [0xD6,0x52,0x03,0x1A,0x2C,0x4E,0x45,0x4B,0x4F,0x00,0x1C,0x00,0x00];
		for( i in 0...firsts.length )
			eq( b.get(i), firsts[i]);
		for( i in 0...lasts.length )
			eq( b.get(b.length - lasts.length + i), lasts[i] );
	}

	#if neko
	static function main() {
		var ch = sys.io.File.write("res1.txt",true);
		ch.writeString(STR);
		ch.close();
		var ch = sys.io.File.write("res2.bin",true);
		ch.writeString("Héllo");
		ch.writeByte(0);
		ch.writeString("World");
		ch.writeInt32(0);
		ch.writeString("!");
		ch.close();
	}
	#end

}