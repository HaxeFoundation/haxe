package cs.system.net.sockets;

/** The exception that is thrown when a socket error occurs. */
@:native("System.Net.Sockets.SocketException")
extern class SocketException extends cs.system.componentmodel.Win32Exception {
	/**
	 * Gets the error code that is associated with this exception.
	 * @return An integer error code that is associated with this exception.
	 */
	var SocketErrorCode(default, never):cs.system.net.sockets.SocketError;
	@:overload(function():Void {})
	function new(errorCode:Int):Void;
}
