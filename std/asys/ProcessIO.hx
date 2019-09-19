package asys;

enum ProcessIO {
	Ignore;
	Inherit;
	Pipe(readable:Bool, writable:Bool, ?pipe:asys.net.Socket);
	Ipc;
	// Stream(_);
	// Fd(_);
}
