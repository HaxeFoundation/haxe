package rust.io;

@:native("std.io.SeekStyle") extern enum SeekStyle {
	SeekSet;
	SeekEnd;
	SeekCur;
}