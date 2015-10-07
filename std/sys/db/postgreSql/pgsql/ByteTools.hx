package sys.db.postgreSql.pgsql ;
import haxe.io.Bytes;
class ByteTools {
	public static function setInt32(byte:Bytes, pos: Int, val:Int) {
		byte.set(pos     , val >> 24);
		byte.set(pos + 1 , val >> 16);
		byte.set(pos + 2 , val >> 8 );
		byte.set(pos + 3 , val		);
	}
}
