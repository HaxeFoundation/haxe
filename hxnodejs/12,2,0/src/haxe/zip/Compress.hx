package haxe.zip;

class Compress {
	public function new(level:Int) {
		throw "Not implemented for this platform";
	}

	public function execute(src:haxe.io.Bytes, srcPos:Int, dst:haxe.io.Bytes, dstPos:Int):{done:Bool, read:Int, write:Int} {
		return null;
	}

	public function setFlushMode(f:FlushMode) {}

	public function close() {}

	public static function run(s:haxe.io.Bytes, level:Int):haxe.io.Bytes {
		var buffer = js.node.Zlib.deflateSync(js.node.Buffer.hxFromBytes(s), {level: level});
		return buffer.hxToBytes();
	}
}
