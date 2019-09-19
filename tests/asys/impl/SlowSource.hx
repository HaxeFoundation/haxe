package impl;

import haxe.io.*;
import haxe.io.Readable.ReadResult;

class SlowSource extends Readable {
	final data:Array<Bytes>;

	public function new(data:Array<Bytes>) {
		super();
		this.data = data.copy();
	}

	override function internalRead(remaining):ReadResult {
		if (data.length > 0) {
			var nextChunk = data.shift();
			var nextEof = data.length == 0;
			asys.Timer.delay(() -> asyncRead([nextChunk], nextEof), 10);
		}
		return None;
	}
}
