package rust.io;
import rust.*;
@:native("io.BytesWriter") extern class BytesWriter implements Writer implements WriterUtil {
	public var bytes(default, null):Array<Int>;
	public var pos:Int;
}