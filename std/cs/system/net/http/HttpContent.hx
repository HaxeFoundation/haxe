package cs.system.net.http;

/** A base class representing an HTTP entity body and content headers. */
@:native("System.Net.Http.HttpContent")
extern class HttpContent {
	/**
	 * Gets the HTTP content headers as defined in RFC 2616.
	 * @return The content headers as defined in RFC 2616.
	 */
	var Headers(default, never):cs.system.net.http.headers.HttpContentHeaders;
	@:overload(function(stream:cs.system.io.Stream):cs.system.threading.tasks.Task {})
	/**
	 * Serialize the HTTP content into a stream of bytes and copies it to the stream
	 * object provided as the  parameter.
	 * @param stream The target stream.
	 * @return The task object representing the asynchronous operation.
	 */
	function CopyToAsync(stream:cs.system.io.Stream, context:cs.system.net.TransportContext):cs.system.threading.tasks.Task;
	/** Releases the unmanaged resources and disposes of the managed resources used by the . */
	function Dispose():Void;
	@:overload(function():cs.system.threading.tasks.Task {})
	/**
	 * Serialize the HTTP content to a memory buffer as an asynchronous operation.
	 * @return The task object representing the asynchronous operation.
	 */
	function LoadIntoBufferAsync(maxBufferSize:haxe.Int64):cs.system.threading.tasks.Task;
	/**
	 * Serialize the HTTP content to a byte array as an asynchronous operation.
	 * @return The task object representing the asynchronous operation.
	 */
	function ReadAsByteArrayAsync():cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>>;
	/**
	 * Serialize the HTTP content and return a stream that represents the content as an
	 * asynchronous operation.
	 * @return The task object representing the asynchronous operation.
	 */
	function ReadAsStreamAsync():cs.system.threading.tasks.Task_1<cs.system.io.Stream>;
	/**
	 * Serialize the HTTP content to a string as an asynchronous operation.
	 * @return The task object representing the asynchronous operation.
	 */
	function ReadAsStringAsync():cs.system.threading.tasks.Task_1<String>;
}
