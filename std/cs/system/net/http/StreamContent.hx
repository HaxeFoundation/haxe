package cs.system.net.http;

/** Provides HTTP content based on a stream. */
@:native("System.Net.Http.StreamContent")
extern class StreamContent extends cs.system.net.http.HttpContent {
	@:overload(function(content:cs.system.io.Stream):Void {})
	function new(content:cs.system.io.Stream, bufferSize:Int):Void;
}
