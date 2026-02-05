package cs.system.net.http;

/** A specialty class that allows applications to call the  method on an HTTP handler chain. */
@:native("System.Net.Http.HttpMessageInvoker")
extern class HttpMessageInvoker {
	@:overload(function(handler:cs.system.net.http.HttpMessageHandler):Void {})
	function new(handler:cs.system.net.http.HttpMessageHandler, disposeHandler:Bool):Void;
	/** Releases the unmanaged resources and disposes of the managed resources used by the . */
	function Dispose():Void;
	/**
	 * Send an HTTP request as an asynchronous operation.
	 * @param request The HTTP request message to send.
	 * @param cancellationToken The cancellation token to cancel operation.
	 * @return The task object representing the asynchronous operation.
	 */
	function SendAsync(request:cs.system.net.http.HttpRequestMessage, cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.system.net.http.HttpResponseMessage>;
}
