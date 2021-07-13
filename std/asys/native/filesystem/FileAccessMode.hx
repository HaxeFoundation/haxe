package asys.native.filesystem;

import haxe.exceptions.ArgumentException;
import haxe.exceptions.NotImplementedException;

enum abstract FileAccessMode(Int) to Int {
	/** File exists and is visible for the current process */
	var Exists = 1;
	/** File can be executed bye the current process */
	var Executable = 2;
	/** File can be written by the current process */
	var Writable = 4;
	/** File can be read by the current process */
	var Readable = 8;

	public inline function has(mode:FileAccessMode):Bool {
		return this & mode != 0;
	}

	@:op(A | B) function join(other:FileAccessMode):FileAccessMode;
}