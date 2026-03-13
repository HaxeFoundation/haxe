package haxe.zip;

import js.node.Buffer;
import js.node.Zlib;

class Uncompress {
	var windowBits:Null<Int>;

	public function new(?windowBits:Int) {
		this.windowBits = windowBits;
	}

	public function execute(src:haxe.io.Bytes, srcPos:Int, dst:haxe.io.Bytes, dstPos:Int):{done:Bool, read:Int, write:Int} {
		var src = js.node.Buffer.hxFromBytes(src).slice(srcPos);
		var dst = js.node.Buffer.hxFromBytes(dst);
		var res = cast Zlib.inflateRawSync(src, cast {info: true, /* windowBits: windowBits */});
		var engine = res.engine;
		var res:Buffer = res.buffer;
		dst.set(res, dstPos);
		return {done: true, read: engine.bytesRead, write: res.byteLength};
	}

	public function setFlushMode(f:FlushMode) {}

	public function close() {}

	public static function run(src:haxe.io.Bytes, ?bufsize:Int):haxe.io.Bytes {
		var buffer = js.node.Zlib.inflateSync(js.node.Buffer.hxFromBytes(src), bufsize == null ? {} : {chunkSize: bufsize});
		return buffer.hxToBytes();
	}
}
