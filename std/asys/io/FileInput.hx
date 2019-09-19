package asys.io;

import haxe.io.Bytes;

class FileInput extends haxe.io.Input {
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

	override public function readByte():Int {
		var buf = Bytes.alloc(1);
		file.readBuffer(buf, 0, 1, position++);
		return buf.get(0);
	}

	override public function readBytes(buf:Bytes, pos:Int, len:Int):Int {
		if (pos < 0 || len < 0 || pos + len > buf.length)
			throw haxe.io.Error.OutsideBounds;
		var read = file.readBuffer(buf, pos, len, position).bytesRead;
		position += read;
		return read;
	}

	override public function close():Void {
		file.close();
	}
}
