package cs.system.net.http;

/** A base type for HTTP message handlers. */
@:native("System.Net.Http.HttpMessageHandler")
extern class HttpMessageHandler {
	/** Releases the unmanaged resources and disposes of the managed resources used by the . */
	function Dispose():Void;
}
