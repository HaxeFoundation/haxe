package cs.system.net;

/** Describes an incoming HTTP request to an  object. This class cannot be inherited. */
@:native("System.Net.HttpListenerRequest")
extern class HttpListenerRequest {
	/**
	 * Gets the MIME types accepted by the client.
	 * @return A  array that contains the type names specified in the request's  header
	 * or  if the client request did not include an  header.
	 */
	var AcceptTypes(default, never):cs.NativeArray<String>;
	/**
	 * Gets an error code that identifies a problem with the  provided by the client.
	 * @return An  value that contains a Windows error code.
	 */
	var ClientCertificateError(default, never):Int;
	/**
	 * Gets the content encoding that can be used with data sent with the request
	 * @return An  object suitable for use with the data in the  property.
	 */
	var ContentEncoding(default, never):cs.system.text.Encoding;
	/**
	 * Gets the length of the body data included in the request.
	 * @return The value from the request's  header. This value is -1 if the content
	 * length is not known.
	 */
	var ContentLength64(default, never):haxe.Int64;
	/**
	 * Gets the MIME type of the body data included in the request.
	 * @return A  that contains the text of the request's  header.
	 */
	var ContentType(default, never):String;
	/**
	 * Gets the cookies sent with the request.
	 * @return A  that contains cookies that accompany the request. This property
	 * returns an empty collection if the request does not contain cookies.
	 */
	var Cookies(default, never):cs.system.net.CookieCollection;
	/**
	 * Gets a  value that indicates whether the request has associated body data.
	 * @return if the request has associated body data; otherwise, .
	 */
	var HasEntityBody(default, never):Bool;
	/**
	 * Gets the collection of header name/value pairs sent in the request.
	 * @return A  that contains the HTTP headers included in the request.
	 */
	var Headers(default, never):cs.system.collections.specialized.NameValueCollection;
	/**
	 * Gets the HTTP method specified by the client.
	 * @return A  that contains the method used in the request.
	 */
	var HttpMethod(default, never):String;
	/**
	 * Gets a stream that contains the body data sent by the client.
	 * @return A readable  object that contains the bytes sent by the client in the
	 * body of the request. This property returns  if no data is sent with the request.
	 */
	var InputStream(default, never):cs.system.io.Stream;
	/**
	 * Gets a  value that indicates whether the client sending this request is
	 * authenticated.
	 * @return if the client was authenticated; otherwise, .
	 */
	var IsAuthenticated(default, never):Bool;
	/**
	 * Gets a  value that indicates whether the request is sent from the local
	 * computer.
	 * @return if the request originated on the same computer as the  object that
	 * provided the request; otherwise, .
	 */
	var IsLocal(default, never):Bool;
	/**
	 * Gets a  value that indicates whether the TCP connection used to send the request
	 * is using the Secure Sockets Layer (SSL) protocol.
	 * @return if the TCP connection is using SSL; otherwise, .
	 */
	var IsSecureConnection(default, never):Bool;
	/**
	 * Gets a  value that indicates whether the TCP connection was  a WebSocket
	 * request.
	 * @return Returns . if the TCP connection is a WebSocket request; otherwise, .
	 */
	var IsWebSocketRequest(default, never):Bool;
	/**
	 * Gets a  value that indicates whether the client requests a persistent
	 * connection.
	 * @return if the connection should be kept open; otherwise, .
	 */
	var KeepAlive(default, never):Bool;
	/**
	 * Gets the server IP address and port number to which the request is directed.
	 * @return An  that represents the IP address that the request is sent to.
	 */
	var LocalEndPoint(default, never):cs.system.net.IPEndPoint;
	/**
	 * Gets the HTTP version used by the requesting client.
	 * @return A  that identifies the client's version of HTTP.
	 */
	var ProtocolVersion(default, never):cs.system.Version;
	/**
	 * Gets the query string included in the request.
	 * @return A  object that contains the query data included in the request .
	 */
	var QueryString(default, never):cs.system.collections.specialized.NameValueCollection;
	/**
	 * Gets the URL information (without the host and port) requested by the client.
	 * @return A  that contains the raw URL for this request.
	 */
	var RawUrl(default, never):String;
	/**
	 * Gets the client IP address and port number from which the request originated.
	 * @return An  that represents the IP address and port number from which the
	 * request originated.
	 */
	var RemoteEndPoint(default, never):cs.system.net.IPEndPoint;
	/**
	 * Gets the request identifier of the incoming HTTP request.
	 * @return A  object that contains the identifier of the HTTP request.
	 */
	var RequestTraceIdentifier(default, never):cs.system.Guid;
	/**
	 * Gets the Service Provider Name (SPN) that the client sent on the request.
	 * @return A  that contains the SPN the client sent on the request.
	 */
	var ServiceName(default, never):String;
	/**
	 * Gets the  for the client request.
	 * @return A  object for the client request.
	 */
	var TransportContext(default, never):cs.system.net.TransportContext;
	/**
	 * Gets the  object requested by the client.
	 * @return A  object that identifies the resource requested by the client.
	 */
	var Url(default, never):cs.system.Uri;
	/**
	 * Gets the Uniform Resource Identifier (URI) of the resource that referred the
	 * client to the server.
	 * @return A  object that contains the text of the request's  header, or  if the
	 * header was not included in the request.
	 */
	var UrlReferrer(default, never):cs.system.Uri;
	/**
	 * Gets the user agent presented by the client.
	 * @return A  object that contains the text of the request's  header.
	 */
	var UserAgent(default, never):String;
	/**
	 * Gets the server IP address and port number to which the request is directed.
	 * @return A  that contains the host address information.
	 */
	var UserHostAddress(default, never):String;
	/**
	 * Gets the DNS name and, if provided, the port number specified by the client.
	 * @return A  value that contains the text of the request's  header.
	 */
	var UserHostName(default, never):String;
	/**
	 * Gets the natural languages that are preferred for the response.
	 * @return A  array that contains the languages specified in the request's  header
	 * or  if the client request did not include an  header.
	 */
	var UserLanguages(default, never):cs.NativeArray<String>;
	/**
	 * Begins an asynchronous request for the client's X.509 v.3 certificate.
	 * @param requestCallback An  delegate that references the method to invoke when
	 * the operation is complete.
	 * @param state A user-defined object that contains information about the
	 * operation. This object is passed to the callback delegate when the operation
	 * completes.
	 * @return An  that indicates the status of the operation.
	 */
	function BeginGetClientCertificate(requestCallback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Ends an asynchronous request for the client's X.509 v.3 certificate.
	 * @param asyncResult The pending request for the certificate.
	 * @return The  object that is returned when the operation started.
	 */
	function EndGetClientCertificate(asyncResult:cs.system.IAsyncResult):cs.system.security.cryptography.x509certificates.X509Certificate2;
	/**
	 * Retrieves the client's X.509 v.3 certificate.
	 * @return A  object that contains the client's X.509 v.3 certificate.
	 */
	function GetClientCertificate():cs.system.security.cryptography.x509certificates.X509Certificate2;
	/**
	 * Retrieves the client's X.509 v.3 certificate as an asynchronous operation.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns a  object that contains the client's X.509 v.3
	 * certificate.
	 */
	function GetClientCertificateAsync():cs.system.threading.tasks.Task_1<cs.system.security.cryptography.x509certificates.X509Certificate2>;
}
