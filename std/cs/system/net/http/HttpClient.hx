package cs.system.net.http;

/** Provides a base class for sending HTTP requests and receiving HTTP responses from a resource identified by a URI. */
@:native("System.Net.Http.HttpClient")
extern class HttpClient extends cs.system.net.http.HttpMessageInvoker {
	/**
	 * Gets or sets the base address of Uniform Resource Identifier (URI) of the
	 * Internet resource used when sending requests.
	 * @return The base address of Uniform Resource Identifier (URI) of the Internet
	 * resource used when sending requests.
	 */
	var BaseAddress(default, default):cs.system.Uri;
	/**
	 * Gets the headers which should be sent with each request.
	 * @return The headers which should be sent with each request.
	 */
	var DefaultRequestHeaders(default, never):cs.system.net.http.headers.HttpRequestHeaders;
	/**
	 * Gets or sets the maximum number of bytes to buffer when reading the response
	 * content.
	 * @return The maximum number of bytes to buffer when reading the response content.
	 * The default value for this property is 2 gigabytes.
	 */
	var MaxResponseContentBufferSize(default, default):haxe.Int64;
	/**
	 * Gets or sets the timespan to wait before the request times out.
	 * @return The timespan to wait before the request times out.
	 */
	var Timeout(default, default):cs.system.TimeSpan;
	@:overload(function():Void {})
	@:overload(function(handler:cs.system.net.http.HttpMessageHandler):Void {})
	function new(handler:cs.system.net.http.HttpMessageHandler, disposeHandler:Bool):Void;
	/** Cancel all pending requests on this instance. */
	function CancelPendingRequests():Void;
	@:overload(function(requestUri:String):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(requestUri:cs.system.Uri):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(requestUri:String, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	/**
	 * Send a DELETE request to the specified Uri as an asynchronous operation.
	 * @param requestUri The Uri the request is sent to.
	 * @return The task object representing the asynchronous operation.
	 */
	function DeleteAsync(requestUri:cs.system.Uri, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage>;
	@:overload(function(requestUri:String):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(requestUri:cs.system.Uri):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(requestUri:String, completionOption:cs.system.net.http.HttpCompletionOption):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(requestUri:String, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(requestUri:cs.system.Uri, completionOption:cs.system.net.http.HttpCompletionOption):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(requestUri:cs.system.Uri, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(requestUri:String, completionOption:cs.system.net.http.HttpCompletionOption, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	/**
	 * Send a GET request to the specified Uri as an asynchronous operation.
	 * @param requestUri The Uri the request is sent to.
	 * @return The task object representing the asynchronous operation.
	 */
	function GetAsync(requestUri:cs.system.Uri, completionOption:cs.system.net.http.HttpCompletionOption, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage>;
	@:overload(function(requestUri:String):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>> {})
	/**
	 * Sends a GET request to the specified Uri and return the response body as a byte
	 * array in an asynchronous operation.
	 * @param requestUri The Uri the request is sent to.
	 * @return The task object representing the asynchronous operation.
	 */
	function GetByteArrayAsync(requestUri:cs.system.Uri):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>>;
	@:overload(function(requestUri:String):cs.system.threading.tasks.Task_1<cs.system.io.Stream> {})
	/**
	 * Send a GET request to the specified Uri and return the response body as a stream
	 * in an asynchronous operation.
	 * @param requestUri The Uri the request is sent to.
	 * @return The task object representing the asynchronous operation.
	 */
	function GetStreamAsync(requestUri:cs.system.Uri):cs.system.threading.tasks.Task_1<cs.system.io.Stream>;
	@:overload(function(requestUri:String):cs.system.threading.tasks.Task_1<String> {})
	/**
	 * Send a GET request to the specified Uri and return the response body as a string
	 * in an asynchronous operation.
	 * @param requestUri The Uri the request is sent to.
	 * @return The task object representing the asynchronous operation.
	 */
	function GetStringAsync(requestUri:cs.system.Uri):cs.system.threading.tasks.Task_1<String>;
	@:overload(function(requestUri:String, content:cs.system.net.http.HttpContent):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(requestUri:cs.system.Uri, content:cs.system.net.http.HttpContent):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(requestUri:String, content:cs.system.net.http.HttpContent, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	/**
	 * Sends a PATCH request to a Uri designated as a string as an asynchronous
	 * operation.
	 * @param requestUri The Uri the request is sent to.
	 * @param content The HTTP request content sent to the server.
	 * @return The task object representing the asynchronous operation.
	 */
	function PatchAsync(requestUri:cs.system.Uri, content:cs.system.net.http.HttpContent, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage>;
	@:overload(function(requestUri:String, content:cs.system.net.http.HttpContent):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(requestUri:cs.system.Uri, content:cs.system.net.http.HttpContent):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(requestUri:String, content:cs.system.net.http.HttpContent, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	/**
	 * Send a POST request to the specified Uri as an asynchronous operation.
	 * @param requestUri The Uri the request is sent to.
	 * @param content The HTTP request content sent to the server.
	 * @return The task object representing the asynchronous operation.
	 */
	function PostAsync(requestUri:cs.system.Uri, content:cs.system.net.http.HttpContent, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage>;
	@:overload(function(requestUri:String, content:cs.system.net.http.HttpContent):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(requestUri:cs.system.Uri, content:cs.system.net.http.HttpContent):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(requestUri:String, content:cs.system.net.http.HttpContent, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	/**
	 * Send a PUT request to the specified Uri as an asynchronous operation.
	 * @param requestUri The Uri the request is sent to.
	 * @param content The HTTP request content sent to the server.
	 * @return The task object representing the asynchronous operation.
	 */
	function PutAsync(requestUri:cs.system.Uri, content:cs.system.net.http.HttpContent, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage>;
	@:overload(function(request:cs.system.net.http.HttpRequestMessage):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(request:cs.system.net.http.HttpRequestMessage, completionOption:cs.system.net.http.HttpCompletionOption):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	@:overload(function(request:cs.system.net.http.HttpRequestMessage, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage> {})
	/**
	 * Send an HTTP request as an asynchronous operation.
	 * @param request The HTTP request message to send.
	 * @return The task object representing the asynchronous operation.
	 */
	function SendAsync(request:cs.system.net.http.HttpRequestMessage, completionOption:cs.system.net.http.HttpCompletionOption, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage>;
}
