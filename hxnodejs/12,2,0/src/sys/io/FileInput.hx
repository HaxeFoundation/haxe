package sys.io;

import haxe.io.Bytes;
import haxe.io.Eof;
import haxe.io.Error;
import js.node.Buffer;
import js.node.Fs;

@:coreApi
class FileInput extends haxe.io.Input {
	var fd:Int;
	var pos:Int;

	var hasReachedEof = false;

	inline function throwEof():Void {
		hasReachedEof = true;
		throw new Eof();
	}

	@:allow(sys.io.File)
	function new(fd:Int) {
		this.fd = fd;
		pos = 0;
	}

	override public function readByte():Int {
		var buf = Buffer.alloc(1);
		var bytesRead = try {
			Fs.readSync(fd, buf, 0, 1, pos);
		} catch (e:Dynamic) {
			if (e.code == "EOF")
				throwEof();
			throw Error.Custom(e);
		}
		if (bytesRead == 0)
			throwEof();
		pos++;
		return buf[0];
	}

	override public function readBytes(s:Bytes, pos:Int, len:Int):Int {
		var buf = Buffer.hxFromBytes(s);
		var bytesRead = try {
			Fs.readSync(fd, buf, pos, len, this.pos);
		} catch (e:Dynamic) {
			if (e.code == "EOF")
				throwEof();
			throw Error.Custom(e);
		}
		if (bytesRead == 0)
			throwEof();
		this.pos += bytesRead;
		return bytesRead;
	}

	override public function close():Void {
		Fs.closeSync(fd);
	}

	public function seek(p:Int, pos:FileSeek):Void {
		hasReachedEof = false;
		switch (pos) {
			case SeekBegin:
				this.pos = p;
			case SeekEnd:
				this.pos = cast Fs.fstatSync(fd).size + p;
			case SeekCur:
				this.pos += p;
		}
	}

	public function tell():Int {
		return pos;
	}

	public function eof():Bool {
		return hasReachedEof;
	}
}
