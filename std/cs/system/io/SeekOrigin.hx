package cs.system.io;

@:native("System.IO.SeekOrigin")
extern enum abstract SeekOrigin(Int) {
	var Begin;
	var Current;
	var End;
}
