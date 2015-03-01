package python.lib.codecs;

import python.lib.Bytes;
import python.lib.Tuple.Tup2;

@:pythonImport("codecs", "Codec")
extern class Codec implements ICodec {
	public function encode(input:Dynamic, ?errors:String = "strict"):Tup2<String, Int>;
	public function decode(input:Dynamic, ?errors:String = "strict"):Tup2<Bytes, Int>;
}

@:remove extern interface ICodec {
	public function encode(input:Dynamic, ?errors:String = "strict"):Tup2<String, Int>;
	public function decode(input:Dynamic, ?errors:String = "strict"):Tup2<Bytes, Int>;
}
