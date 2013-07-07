package rust.io.FileFlag;

@:extern ("io.FileFlag") extern enum FileFlag {
	Append;
	Create;
	Truncate;
	NoFlag;
}