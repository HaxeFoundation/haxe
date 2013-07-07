package rust.io;
import haxe.io.Output;
class NativeOutput extends Output {
	public var _(default, null):Writer;
	public function new(writer:Writer) {
		_ = writer;
	}
	public override function writeByte(b:Int):Void {
		_.write([b]);
	}
}