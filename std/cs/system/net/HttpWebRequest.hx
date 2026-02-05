package cs.system.net;

/** Provides an HTTP-specific implementation of the  class. */
@:native("System.Net.HttpWebRequest")
extern class HttpWebRequest extends cs.system.net.WebRequest {
	/**
	 * Gets or sets the default maximum length of an HTTP error response.
	 * @return The default maximum length of an HTTP error response.
	 */
	static var DefaultMaximumErrorResponseLength(default, default):Int;
	/**
	 * Gets or sets the default for the  property.
	 * @return The length, in kilobytes (1024 bytes), of the default maximum for
	 * response headers received. The default configuration file sets this value to 64
	 * kilobytes.
	 */
	static var DefaultMaximumResponseHeadersLength(default, default):Int;
	/**
	 * Gets or sets the value of the  HTTP header.
	 * @return The value of the  HTTP header. The default value is .
	 */
	var Accept(default, default):String;
	/**
	 * Gets the Uniform Resource Identifier (URI) of the Internet resource that
	 * actually responds to the request.
	 * @return A  that identifies the Internet resource that actually responds to the
	 * request. The default is the URI used by the  method to initialize the request.
	 */
	var Address(default, never):cs.system.Uri;
	/**
	 * Gets or sets a value that indicates whether the request should follow
	 * redirection responses.
	 * @return if the request should automatically follow redirection responses from
	 * the Internet resource; otherwise, . The default value is .
	 */
	var AllowAutoRedirect(default, default):Bool;
	/**
	 * Gets or sets a value that indicates whether to buffer the received from the
	 * Internet resource.
	 * @return to enable buffering of the data received from the Internet resource;  to
	 * disable buffering. The default is .
	 */
	var AllowReadStreamBuffering(default, default):Bool;
	/**
	 * Gets or sets a value that indicates whether to buffer the data sent to the
	 * Internet resource.
	 * @return to enable buffering of the data sent to the Internet resource;  to
	 * disable buffering. The default is .
	 */
	var AllowWriteStreamBuffering(default, default):Bool;
	/**
	 * Gets or sets the type of decompression that is used.
	 * @return A  object that indicates the type of decompression that is used.
	 */
	var AutomaticDecompression(default, default):cs.system.net.DecompressionMethods;
	/**
	 * Gets or sets the collection of security certificates that are associated with
	 * this request.
	 * @return The  that contains the security certificates associated with this
	 * request.
	 */
	var ClientCertificates(default, default):cs.system.security.cryptography.x509certificates.X509CertificateCollection;
	/**
	 * Gets or sets the value of the  HTTP header.
	 * @return The value of the  HTTP header. The default value is .
	 */
	var Connection(default, default):String;
	/**
	 * Gets or sets the delegate method called when an HTTP 100-continue response is
	 * received from the Internet resource.
	 * @return A delegate that implements the callback method that executes when an
	 * HTTP Continue response is returned from the Internet resource. The default value
	 * is .
	 */
	var ContinueDelegate(default, default):cs.system.net.HttpContinueDelegate;
	/**
	 * Gets or sets a timeout, in milliseconds, to wait until the 100-Continue is
	 * received from the server.
	 * @return The timeout, in milliseconds, to wait until the 100-Continue is
	 * received.
	 */
	var ContinueTimeout(default, default):Int;
	/**
	 * Gets or sets the cookies associated with the request.
	 * @return A  that contains the cookies associated with this request.
	 */
	var CookieContainer(default, default):cs.system.net.CookieContainer;
	/**
	 * Gets or sets the  HTTP header value to use in an HTTP request.
	 * @return The Date header value in the HTTP request.
	 */
	var Date(default, default):cs.system.DateTime;
	/**
	 * Gets or sets the value of the  HTTP header.
	 * @return The contents of the  HTTP header. The default value is . The value for
	 * this property is stored in . If WebHeaderCollection is set, the property value
	 * is lost.
	 */
	var Expect(default, default):String;
	/**
	 * Gets a value that indicates whether a response has been received from an
	 * Internet resource.
	 * @return if a response has been received; otherwise, .
	 */
	var HaveResponse(default, never):Bool;
	/**
	 * Gets or sets the Host header value to use in an HTTP request independent from
	 * the request URI.
	 * @return The Host header value in the HTTP request.
	 */
	var Host(default, default):String;
	/**
	 * Gets or sets the value of the  HTTP header.
	 * @return A  that contains the contents of the  HTTP header. The default value is
	 * the current date and time.
	 */
	var IfModifiedSince(default, default):cs.system.DateTime;
	/**
	 * Gets or sets a value that indicates whether to make a persistent connection to
	 * the Internet resource.
	 * @return if the request to the Internet resource should contain a  HTTP header
	 * with the value Keep-alive; otherwise, . The default is .
	 */
	var KeepAlive(default, default):Bool;
	/**
	 * Gets or sets the maximum number of redirects that the request follows.
	 * @return The maximum number of redirection responses that the request follows.
	 * The default value is 50.
	 */
	var MaximumAutomaticRedirections(default, default):Int;
	/**
	 * Gets or sets the maximum allowed length of the response headers.
	 * @return The length, in kilobytes (1024 bytes), of the response headers.
	 */
	var MaximumResponseHeadersLength(default, default):Int;
	/**
	 * Gets or sets the media type of the request.
	 * @return The media type of the request. The default value is .
	 */
	var MediaType(default, default):String;
	/**
	 * Gets or sets a value that indicates whether to pipeline the request to the
	 * Internet resource.
	 * @return if the request should be pipelined; otherwise, . The default is .
	 */
	var Pipelined(default, default):Bool;
	/**
	 * Gets or sets the version of HTTP to use for the request.
	 * @return The HTTP version to use for the request. The default is .
	 */
	var ProtocolVersion(default, default):cs.system.Version;
	/**
	 * Gets or sets a time-out in milliseconds when writing to or reading from a
	 * stream.
	 * @return The number of milliseconds before the writing or reading times out. The
	 * default value is 300,000 milliseconds (5 minutes).
	 */
	var ReadWriteTimeout(default, default):Int;
	/**
	 * Gets or sets the value of the  HTTP header.
	 * @return The value of the  HTTP header. The default value is .
	 */
	var Referer(default, default):String;
	/**
	 * Gets or sets a value that indicates whether to send data in segments to the
	 * Internet resource.
	 * @return to send data to the Internet resource in segments; otherwise, . The
	 * default value is .
	 */
	var SendChunked(default, default):Bool;
	/**
	 * Gets or sets a callback function to validate the server certificate.
	 * @return A callback function to validate the server certificate.
	 */
	var ServerCertificateValidationCallback(default, default):cs.system.net.security.RemoteCertificateValidationCallback;
	/**
	 * Gets the service point to use for the request.
	 * @return A  that represents the network connection to the Internet resource.
	 */
	var ServicePoint(default, never):cs.system.net.ServicePoint;
	/**
	 * Gets a value that indicates whether the request provides support for a .
	 * @return if the request provides support for a ; otherwise, .
	 */
	var SupportsCookieContainer(default, never):Bool;
	/**
	 * Gets or sets the value of the  HTTP header.
	 * @return The value of the  HTTP header. The default value is .
	 */
	var TransferEncoding(default, default):String;
	/**
	 * Gets or sets a value that indicates whether to allow high-speed
	 * NTLM-authenticated connection sharing.
	 * @return to keep the authenticated connection open; otherwise, .
	 */
	var UnsafeAuthenticatedConnectionSharing(default, default):Bool;
	/**
	 * Gets or sets the value of the  HTTP header.
	 * @return The value of the  HTTP header. The default value is . The value for this
	 * property is stored in . If WebHeaderCollection is set, the property value is
	 * lost.
	 */
	var UserAgent(default, default):String;
	/** Cancels a request to an Internet resource. */
	function Abort():Void;
	@:overload(function(range:Int):Void {})
	@:overload(function(range:haxe.Int64):Void {})
	@:overload(function(from:Int, to:Int):Void {})
	@:overload(function(from:haxe.Int64, to:haxe.Int64):Void {})
	@:overload(function(rangeSpecifier:String, range:Int):Void {})
	@:overload(function(rangeSpecifier:String, range:haxe.Int64):Void {})
	@:overload(function(rangeSpecifier:String, from:Int, to:Int):Void {})
	/**
	 * Adds a byte range header to a request for a specific range from the beginning or
	 * end of the requested data.
	 * @param range The starting or ending point of the range.
	 */
	function AddRange(rangeSpecifier:String, from:haxe.Int64, to:haxe.Int64):Void;
	/**
	 * Begins an asynchronous request for a  object to use to write data.
	 * @param callback The  delegate.
	 * @param state The state object for this request.
	 * @return An  that references the asynchronous request.
	 */
	function BeginGetRequestStream(callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous request to an Internet resource.
	 * @param callback The  delegate
	 * @param state The state object for this request.
	 * @return An  that references the asynchronous request for a response.
	 */
	function BeginGetResponse(callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	@:overload(function(asyncResult:cs.system.IAsyncResult):cs.system.io.Stream {})
	/**
	 * Ends an asynchronous request for a  object to use to write data.
	 * @param asyncResult The pending request for a stream.
	 * @return A  to use to write request data.
	 */
	function EndGetRequestStream(asyncResult:cs.system.IAsyncResult, context:cs.Ref<cs.system.net.TransportContext>):cs.system.io.Stream;
	/**
	 * Ends an asynchronous request to an Internet resource.
	 * @param asyncResult The pending request for a response.
	 * @return A  that contains the response from the Internet resource.
	 */
	function EndGetResponse(asyncResult:cs.system.IAsyncResult):cs.system.net.WebResponse;
	@:overload(function():cs.system.io.Stream {})
	/**
	 * Gets a  object to use to write request data.
	 * @return A  to use to write request data.
	 */
	function GetRequestStream(context:cs.Ref<cs.system.net.TransportContext>):cs.system.io.Stream;
	/**
	 * Returns a response from an Internet resource.
	 * @return A  that contains the response from the Internet resource.
	 */
	function GetResponse():cs.system.net.WebResponse;
}
