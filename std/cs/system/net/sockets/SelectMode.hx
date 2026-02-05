package cs.system.net.sockets;

/** Defines the polling modes for the  method. */
@:native("System.Net.Sockets.SelectMode")
extern enum SelectMode {
	SelectError;
	SelectRead;
	SelectWrite;
}
