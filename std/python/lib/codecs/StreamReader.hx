package python.lib.codecs;

import python.lib.codecs.Codec;

@:pythonImport("codecs", "StreamReader")
extern class StreamReader extends Codec implements IStreamReader {
	public function read(?size:Int, ?chars:Int, ?firstline:Bool):String;
	public function readline(?size:Int, ?keepsend:Bool = false):String;
	public function readlines(?sizehint:Int, ?keepsend:Bool = false):Array<String>;
	public function reset():Void;
}

extern interface IStreamReader extends ICodec {
	public function read(?size:Int, ?chars:Int, ?firstline:Bool):String;
	public function readline(?size:Int, ?keepsend:Bool = false):String;
	public function readlines(?sizehint:Int, ?keepsend:Bool = false):Array<String>;
	public function reset():Void;
}