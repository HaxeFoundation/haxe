package haxe.crypto;

class Crc32 {

	/**
		Calculates the CRC32 of the given data bytes
	**/
	public static function make( data : haxe.io.Bytes ) : Int {
		var init = 0xFFFFFFFF;
		var polynom = 0xEDB88320;
		var crc = init;
		var b = data.getData();
		for( i in 0...data.length ) {
			var tmp = (crc ^ haxe.io.Bytes.fastGet(b,i)) & 0xFF;
			for( j in 0...8 ) {
				if( tmp & 1 == 1 )
					tmp = (tmp >>> 1) ^ polynom;
				else
					tmp >>>= 1;
			}
			crc = (crc >>> 8) ^ tmp;
		}
		return crc ^ init;
	}

}