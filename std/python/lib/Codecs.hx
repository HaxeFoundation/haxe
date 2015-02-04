package python.lib;

import python.lib.Bytes;
import python.lib.codecs.StreamReaderWriterText;

@:pythonImport("codecs")
extern class Codecs {

	static function open(filename:String, mode:String, ?encoding:String, ?errors:String, ?buffering:Bool):StreamReaderWriterText;
	static function encode(obj:String, ?encoding:String = "utf-8", errors:String = "strict"):Bytes;
	static function decode(obj:Bytes, ?encoding:String = "utf-8", errors:String = "strict"):String;

}