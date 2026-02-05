package cs.system.io;

/** Specifies the position in a stream to use for seeking. */
@:native("System.IO.SeekOrigin")
extern enum SeekOrigin {
	Begin;
	Current;
	End;
}
