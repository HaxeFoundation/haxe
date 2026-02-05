package cs.system.net.sockets;

/** Defines the polling modes for the  method. */
@:native("System.Net.Sockets.SelectMode")
extern enum abstract SelectMode(Int) {
	var SelectError = 2;
	var SelectRead = 0;
	var SelectWrite = 1;
}
