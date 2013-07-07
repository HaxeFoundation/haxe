package rust.io;

@:native("std.io.WriterType") extern enum WriterType {
	Screen;
	File;
}