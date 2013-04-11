package rust.io;

@:native("io.SeekStyle") extern enum SeekStyle {
	SeekSet;
	SeekEnd;
	SeekCur;
}