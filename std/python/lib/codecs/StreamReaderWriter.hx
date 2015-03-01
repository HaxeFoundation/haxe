package python.lib.codecs;

import python.lib.codecs.Codec;
import python.lib.codecs.StreamReader;
import python.lib.codecs.StreamWriter;

@:pythonImport("codecs", "StreamReaderWriter")
extern class StreamReaderWriter extends StreamReader implements IStreamWriter {
	public function write(object:Dynamic):Void;
	public function writelines(list:Array<String>):Void;

}

@:remove extern interface IStreamReaderWriter extends IStreamReader extends IStreamWriter {}