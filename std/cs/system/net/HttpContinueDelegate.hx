package cs.system.net;

/**
 * Represents the method that notifies callers when a continue response is received
 * by the client.
 * @param StatusCode The numeric value of the HTTP status from the server.
 * @param httpHeaders The headers returned with the 100-continue response from the
 * server.
 */
@:native("System.Net.HttpContinueDelegate")
extern class HttpContinueDelegate extends cs.system.MulticastDelegate {
	function new(func:(StatusCode:Int, httpHeaders:cs.system.net.WebHeaderCollection)->Void):Void;
	function Invoke(StatusCode:Int, httpHeaders:cs.system.net.WebHeaderCollection):Void;
}
