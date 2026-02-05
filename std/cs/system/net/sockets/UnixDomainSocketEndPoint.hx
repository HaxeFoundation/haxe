package cs.system.net.sockets;

/** Represents a Unix Domain Socket endpoint as a path. */
@:native("System.Net.Sockets.UnixDomainSocketEndPoint")
extern class UnixDomainSocketEndPoint extends cs.system.net.EndPoint {
	function new(path:String):Void;
}
