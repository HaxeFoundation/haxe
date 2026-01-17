package cs.system.net.sockets;

import cs.system.io.Stream;

@:native("System.Net.Sockets.NetworkStream")
extern class NetworkStream extends Stream {
	function new(socket:Socket):Void;
}
