package python.lib.codecs;

import python.lib.codecs.Codec;

extern interface StreamReader extends Codec {
	public function read(?size:Int, ?chars:Int, ?firstline:Bool):String;
	public function readline(?size:Int, ?keepsend:Bool = false):String;
	public function readlines(?sizehint:Int, ?keepsend:Bool = false):Array<String>;
	public function reset():Void;

}