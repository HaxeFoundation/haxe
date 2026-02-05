package cs.system.io;

/** Specifies how the operating system should open a file. */
@:native("System.IO.FileMode")
extern enum abstract FileMode(Int) {
	var Append = 6;
	var Create = 2;
	var CreateNew = 1;
	var Open = 3;
	var OpenOrCreate = 4;
	var Truncate = 5;
}
