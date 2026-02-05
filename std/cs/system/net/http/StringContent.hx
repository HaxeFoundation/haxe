package cs.system.net.http;

/** Provides HTTP content based on a string. */
@:native("System.Net.Http.StringContent")
extern class StringContent extends cs.system.net.http.ByteArrayContent {
	@:overload(function(content:String):Void {})
	@:overload(function(content:String, encoding:cs.system.text.Encoding):Void {})
	function new(content:String, encoding:cs.system.text.Encoding, mediaType:String):Void;
}
