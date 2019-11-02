package asys.io;

import haxe.io.Bytes;

class FileOutput extends haxe.io.Output {
	final file:asys.io.File;
	var position:Int = 0;

	function new(file:asys.io.File) {
		this.file = file;
	}

	public function seek(p:Int, pos:sys.io.FileSeek):Void {
		position = (switch (pos) {
			case SeekBegin: p;
			case SeekCur: position + p;
			case SeekEnd: file.stat().size + p;
		});
	}

	public function tell():Int {
		return position;
	}

	override public function writeByte(byte:Int):Void {
		var buf = Bytes.alloc(1);
		buf.set(1, byte);
		file.writeBuffer(buf, 0, 1, position++);
	}

	override public function writeBytes(buf:Bytes, pos:Int, len:Int):Int {
		if (pos < 0 || len < 0 || pos + len > buf.length)
			throw haxe.io.Error.OutsideBounds;
		var written = file.writeBuffer(buf, pos, len, position).bytesWritten;
		position += written;
		return written;
	}

	override public function flush():Void {
		file.datasync();
	}

	override public function close():Void {
		file.close();
	}
}
