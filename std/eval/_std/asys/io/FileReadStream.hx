package asys.io;

import haxe.io.*;
import haxe.io.Readable.ReadResult;

typedef FileReadStreamOptions = {
	?autoClose:Bool,
	?start:Int,
	?end:Int,
	?highWaterMark:Int
};

class FileReadStream extends Readable {
	final file:File;
	var position:Int;
	final end:Int;
	var readInProgress:Bool = false;

	public function new(file:File, ?options:FileReadStreamOptions) {
		super();
		if (options == null)
			options = {};
		this.file = file;
		position = options.start != null ? options.start : 0;
		end = options.end != null ? options.end : 0xFFFFFFFF;
	}

	override function internalRead(remaining):ReadResult {
		if (readInProgress)
			return None;
		readInProgress = true;
		// TODO: check errors
		var chunk = Bytes.alloc(remaining);
		// TODO: check EOF for file as well
		var willEnd = (position + remaining) >= end;
		file.async.readBuffer(chunk, 0, remaining, position, (err, _) -> {
			readInProgress = false;
			if (err != null)
				errorSignal.emit(err);
			asyncRead([chunk], willEnd);
		});
		position += remaining;
		return None;
	}
}
