package python.lib.codecs;

import python.lib.Bytes;
import python.lib.Tuple.Tup2;
import python.lib.codecs.StreamReader;
import python.lib.codecs.StreamWriter;

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