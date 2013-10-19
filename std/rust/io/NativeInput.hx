package rust.io;
import haxe.io.Input;
class NativeInput extends Input {
	public var _(default, null):Reader;
	public function new(reader:Reader) {
		_ = reader;
	}
	public override function readByte():Int {
		return _.read_byte();
	}
}