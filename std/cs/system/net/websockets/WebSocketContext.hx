package cs.system.net.websockets;

/** Used for accessing the information in the WebSocket handshake. */
@:native("System.Net.WebSockets.WebSocketContext")
extern class WebSocketContext {
	/**
	 * The cookies that were passed to the server during the opening handshake.
	 * @return Returns .
	 */
	var CookieCollection(default, never):cs.system.net.CookieCollection;
	/**
	 * The HTTP headers that were sent to the server during the opening handshake.
	 * @return Returns .
	 */
	var Headers(default, never):cs.system.collections.specialized.NameValueCollection;
	/**
	 * Whether the WebSocket client is authenticated.
	 * @return Returns .
	 */
	var IsAuthenticated(default, never):Bool;
	/**
	 * Whether the WebSocket client connected from the local machine.
	 * @return Returns .
	 */
	var IsLocal(default, never):Bool;
	/**
	 * Whether the WebSocket connection is secured using Secure Sockets Layer (SSL).
	 * @return Returns .
	 */
	var IsSecureConnection(default, never):Bool;
	/**
	 * The value of the Origin HTTP header included in the opening handshake.
	 * @return Returns .
	 */
	var Origin(default, never):String;
	/**
	 * The URI requested by the WebSocket client.
	 * @return Returns .
	 */
	var RequestUri(default, never):cs.system.Uri;
	/**
	 * The value of the SecWebSocketKey HTTP header included in the opening handshake.
	 * @return Returns .
	 */
	var SecWebSocketKey(default, never):String;
	/**
	 * The value of the SecWebSocketKey HTTP header included in the opening handshake.
	 * @return Returns .
	 */
	var SecWebSocketProtocols(default, never):cs.system.collections.generic.IEnumerable<String>;
	/**
	 * The list of subprotocols requested by the WebSocket client.
	 * @return Returns .
	 */
	var SecWebSocketVersion(default, never):String;
	/**
	 * An object used to obtain identity, authentication information, and security
	 * roles for the WebSocket client.
	 * @return Returns .
	 */
	var User(default, never):cs.system.security.principal.IPrincipal;
	/**
	 * The WebSocket instance used to interact (send/receive/close/etc) with the
	 * WebSocket connection.
	 * @return Returns .
	 */
	var WebSocket(default, never):cs.system.net.websockets.WebSocket;
}
