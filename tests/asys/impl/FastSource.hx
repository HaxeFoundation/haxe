package impl;

import haxe.io.*;
import haxe.io.Readable.ReadResult;

class FastSource extends Readable {
	final data:Array<Bytes>;

	public function new(data:Array<Bytes>, ?highWaterMark:Int) {
		super(highWaterMark);
		this.data = data.copy();
	}

	override function internalRead(remaining):ReadResult {
		return Data([data.shift()], data.length == 0);
	}
}
