package cs.system.net.http;

/** Provides HTTP content based on a byte array. */
@:native("System.Net.Http.ByteArrayContent")
extern class ByteArrayContent extends cs.system.net.http.HttpContent {
	@:overload(function(content:cs.NativeArray<cs.UInt8>):Void {})
	function new(content:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
}
