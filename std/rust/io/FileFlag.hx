package rust.io.FileFlag;

@:native ("std.io.FileFlag") extern enum FileFlag {
	Append;
	Create;
	Truncate;
	NoFlag;
}