
package python.lib;

import python.lib.io.IOBase;
import python.lib.io.RawIOBase;
import python.lib.io.TextIOBase;
import python.lib.Bytes;
import python.lib.FileObject;
import python.lib.Tuple.Tup2;

extern interface Codec {
	public function encode(input:Dynamic, ?errors:String = "strict"):Tup2<String, Int>;
	public function decode(input:Dynamic, ?errors:String = "strict"):Tup2<Bytes, Int>;
}

extern interface StreamReader extends Codec {
	public function read(?size:Int, ?chars:Int, ?firstline:Bool):String;
	public function readline(?size:Int, ?keepsend:Bool = false):String;
	public function readlines(?sizehint:Int, ?keepsend:Bool = false):Array<String>;
	public function reset():Void;

}

extern interface StreamWriter extends Codec {
	public function write(object:Dynamic):Void;
	public function writelines(list:Array<String>):Void;
	public function reset():Void;
}

extern class StreamReaderWriterText implements StreamReader implements StreamWriter  {
	public function read(?size:Int, ?chars:Int, ?firstline:Bool):String;
	public function readline(?size:Int, ?keepsend:Bool = false):String;
	public function readlines(?sizehint:Int, ?keepsend:Bool = false):Array<String>;
	public function reset():Void;

	public function write(object:Dynamic):Void;
	public function writelines(list:Array<String>):Void;

	public function close():Void;

	public function encode(input:Dynamic, ?errors:String = "strict"):Tup2<String, Int>;
	public function decode(input:Dynamic, ?errors:String = "strict"):Tup2<Bytes, Int>;
}


@:pythonImport("codecs")
extern class Codecs {

	static function open(filename:String, mode:String, ?encoding:String, ?errors:String, ?buffering:Bool):StreamReaderWriterText;
	static function encode(obj:String, ?encoding:String = "utf-8", errors:String = "strict"):Bytes;
	static function decode(obj:Bytes, ?encoding:String = "utf-8", errors:String = "strict"):String;

}