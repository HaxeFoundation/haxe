package python.lib.codecs;

import python.lib.Bytes;
import python.lib.Tuple.Tup2;

extern interface Codec {
	public function encode(input:Dynamic, ?errors:String = "strict"):Tup2<String, Int>;
	public function decode(input:Dynamic, ?errors:String = "strict"):Tup2<Bytes, Int>;
}
