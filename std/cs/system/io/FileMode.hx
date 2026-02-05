package cs.system.io;

/** Specifies how the operating system should open a file. */
@:native("System.IO.FileMode")
extern enum FileMode {
	Append;
	Create;
	CreateNew;
	Open;
	OpenOrCreate;
	Truncate;
}
