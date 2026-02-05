package cs.system.net;

/** Provides access to the request and response objects used by the  class. This class cannot be inherited. */
@:native("System.Net.HttpListenerContext")
extern class HttpListenerContext {
	/**
	 * Gets the  that represents a client's request for a resource.
	 * @return An  object that represents the client request.
	 */
	var Request(default, never):cs.system.net.HttpListenerRequest;
	/**
	 * Gets the  object that will be sent to the client in response to the client's
	 * request.
	 * @return An  object used to send a response back to the client.
	 */
	var Response(default, never):cs.system.net.HttpListenerResponse;
	/**
	 * Gets an object used to obtain identity, authentication information, and security
	 * roles for the client whose request is represented by this  object.
	 * @return An  object that describes the client, or  if the  that supplied this 
	 * does not require authentication.
	 */
	var User(default, never):cs.system.security.principal.IPrincipal;
	@:overload(function(subProtocol:String):cs.system.threading.tasks.Task_1<cs.system.net.websockets.HttpListenerWebSocketContext> {})
	@:overload(function(subProtocol:String, keepAliveInterval:cs.system.TimeSpan):cs.system.threading.tasks.Task_1<cs.system.net.websockets.HttpListenerWebSocketContext> {})
	@:overload(function(subProtocol:String, receiveBufferSize:Int, keepAliveInterval:cs.system.TimeSpan):cs.system.threading.tasks.Task_1<cs.system.net.websockets.HttpListenerWebSocketContext> {})
	/**
	 * Accept a WebSocket connection as an asynchronous operation.
	 * @param subProtocol The supported WebSocket sub-protocol.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns an  object.
	 */
	function AcceptWebSocketAsync(subProtocol:String, receiveBufferSize:Int, keepAliveInterval:cs.system.TimeSpan, internalBuffer:cs.system.ArraySegment<cs.UInt8>):cs.system.threading.tasks.Task_1<cs.system.net.websockets.HttpListenerWebSocketContext>;
}
