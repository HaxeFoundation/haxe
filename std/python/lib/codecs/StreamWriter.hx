package python.lib.codecs;

import python.lib.codecs.Codec;

extern interface StreamWriter extends Codec {
	public function write(object:Dynamic):Void;
	public function writelines(list:Array<String>):Void;
	public function reset():Void;
}