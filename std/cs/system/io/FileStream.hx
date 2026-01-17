package cs.system.io;

import cs.system.io.Stream;

@:native("System.IO.FileStream")
extern class FileStream extends Stream {
	@:overload(function(path:String, mode:FileMode):Void {})
	@:overload(function(path:String, mode:FileMode, access:FileAccess):Void {})
	function new(path:String, mode:FileMode, access:FileAccess, share:FileShare):Void;
}

@:native("System.IO.FileMode")
extern enum abstract FileMode(Int) {
	var CreateNew;
	var Create;
	var Open;
	var OpenOrCreate;
	var Truncate;
	var Append;
}

@:native("System.IO.FileAccess")
extern enum abstract FileAccess(Int) {
	var Read;
	var Write;
	var ReadWrite;
}

@:native("System.IO.FileShare")
extern enum abstract FileShare(Int) {
	var None;
	var Read;
	var Write;
	var ReadWrite;
	var Delete;
	var Inheritable;
}
