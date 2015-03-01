package python.lib.codecs;

import python.lib.codecs.Codec;

@:pythonImport("codecs", "StreamWriter")
extern class StreamWriter extends Codec implements ICodec {
	public function write(object:Dynamic):Void;
	public function writelines(list:Array<String>):Void;
	public function reset():Void;
}


@:remove extern interface IStreamWriter extends ICodec {
	public function write(object:Dynamic):Void;
	public function writelines(list:Array<String>):Void;
	public function reset():Void;
}