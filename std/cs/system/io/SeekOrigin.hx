package cs.system.io;

/** Specifies the position in a stream to use for seeking. */
@:native("System.IO.SeekOrigin")
extern enum abstract SeekOrigin(Int) {
	var Begin = 0;
	var Current = 1;
	var End = 2;
}
