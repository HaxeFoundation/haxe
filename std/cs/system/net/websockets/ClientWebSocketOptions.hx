package cs.system.net.websockets;

/** Options to use with a   object. */
@:native("System.Net.WebSockets.ClientWebSocketOptions")
extern class ClientWebSocketOptions {
	/**
	 * Gets or sets a collection of client side certificates.
	 * @return A collection of client side certificates.
	 */
	var ClientCertificates(default, default):cs.system.security.cryptography.x509certificates.X509CertificateCollection;
	/**
	 * Gets or sets the cookies associated with the request.
	 * @return The cookies associated with the request.
	 */
	var Cookies(default, default):cs.system.net.CookieContainer;
	/**
	 * Gets or sets the credential information for the client.
	 * @return The credential information for the client.
	 */
	var Credentials(default, default):cs.system.net.ICredentials;
	/**
	 * Gets or sets the WebSocket protocol keep-alive interval.
	 * @return The WebSocket protocol keep-alive interval.
	 */
	var KeepAliveInterval(default, default):cs.system.TimeSpan;
	/**
	 * Gets or sets the proxy for WebSocket requests.
	 * @return The proxy for WebSocket requests.
	 */
	var Proxy(default, default):cs.system.net.IWebProxy;
	var RemoteCertificateValidationCallback(default, default):cs.system.net.security.RemoteCertificateValidationCallback;
	/**
	 * Gets or sets a  value that indicates if default credentials should be used
	 * during WebSocket handshake.
	 * @return if default credentials should be used during WebSocket handshake;
	 * otherwise, . The default is .
	 */
	var UseDefaultCredentials(default, default):Bool;
	/**
	 * Adds a sub-protocol to be negotiated during the WebSocket connection handshake.
	 * @param subProtocol The WebSocket sub-protocol to add.
	 */
	function AddSubProtocol(subProtocol:String):Void;
	@:overload(function(receiveBufferSize:Int, sendBufferSize:Int):Void {})
	/**
	 * Sets the client buffer parameters.
	 * @param receiveBufferSize The size, in bytes, of the client receive buffer.
	 * @param sendBufferSize The size, in bytes, of the client send buffer.
	 */
	function SetBuffer(receiveBufferSize:Int, sendBufferSize:Int, buffer:cs.system.ArraySegment<cs.UInt8>):Void;
	/**
	 * Creates a HTTP request header and its value.
	 * @param headerName The name of the HTTP header.
	 * @param headerValue The value of the HTTP header.
	 */
	function SetRequestHeader(headerName:String, headerValue:String):Void;
}
