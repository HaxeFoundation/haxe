package unit;

class TestResource extends Test {

	static var STR = "Héllo World !";

	function testResources() {
		var b = haxe.io.Bytes.ofString("Héllo World    !");
		for( i in 0...b.length )
			if( b.get(i) == " ".charCodeAt(0) )
				b.set(i,0);
		var names = haxe.Resource.listNames();
		eq( names.length, 2 );
		if( names[0] == "res1.txt" )
			eq( names[1], "res2.bin" );
		else {
			eq( names[0], "res2.bin" );
			eq( names[1], "res1.txt" );
		}
		eq( haxe.Resource.getString("res1.txt"), STR );
		#if (neko || flash9 || php)
		// allow binary strings
		eq( haxe.Resource.getString("res2.bin"), "Héllo\000World\000\000\000\000!" );
		#else
		// cut until first \0
		eq( haxe.Resource.getString("res2.bin"), "Héllo" );
		#end
		eq( haxe.Resource.getBytes("res1.txt").compare(haxe.io.Bytes.ofString(STR)), 0 );
		eq( haxe.Resource.getBytes("res2.bin").compare(b), 0 );
	}

	#if neko
	static function main() {
		var ch = neko.io.File.write("res1.txt",true);
		ch.writeString(STR);
		ch.close();
		var ch = neko.io.File.write("res2.bin",true);
		ch.writeString("Héllo");
		ch.writeByte(0);
		ch.writeString("World");
		ch.writeInt31(0);
		ch.writeString("!");
		ch.close();
	}
	#end

}