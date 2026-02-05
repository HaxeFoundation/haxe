package cs.system.net.http;

@:native("System.Net.Http.ReadOnlyMemoryContent")
extern class ReadOnlyMemoryContent extends cs.system.net.http.HttpContent {
	function new(content:cs.system.ReadOnlyMemory<cs.UInt8>):Void;
}
