package cs.system.net.websockets;

/** Represents an exception that occurred when performing an operation on a WebSocket connection. */
@:native("System.Net.WebSockets.WebSocketException")
extern class WebSocketException extends cs.system.componentmodel.Win32Exception {
	/**
	 * Returns a WebSocketError indicating the type of error that occurred.
	 * @return Returns .
	 */
	var WebSocketErrorCode(default, never):cs.system.net.websockets.WebSocketError;
	@:overload(function():Void {})
	@:overload(function(nativeError:Int):Void {})
	@:overload(function(error:cs.system.net.websockets.WebSocketError):Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(nativeError:Int, innerException:cs.system.Exception):Void {})
	@:overload(function(nativeError:Int, message:String):Void {})
	@:overload(function(error:cs.system.net.websockets.WebSocketError, innerException:cs.system.Exception):Void {})
	@:overload(function(error:cs.system.net.websockets.WebSocketError, nativeError:Int):Void {})
	@:overload(function(error:cs.system.net.websockets.WebSocketError, message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	@:overload(function(error:cs.system.net.websockets.WebSocketError, nativeError:Int, innerException:cs.system.Exception):Void {})
	@:overload(function(error:cs.system.net.websockets.WebSocketError, nativeError:Int, message:String):Void {})
	@:overload(function(error:cs.system.net.websockets.WebSocketError, message:String, innerException:cs.system.Exception):Void {})
	function new(error:cs.system.net.websockets.WebSocketError, nativeError:Int, message:String, innerException:cs.system.Exception):Void;
	/**
	 * Sets the SerializationInfo object with the file name and line number where the
	 * exception occurred.
	 * @param info A SerializationInfo object.
	 * @param context The contextual information about the source or destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
