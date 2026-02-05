package cs.system.net.http.headers;

@:native("System.Net.Http.Headers.HttpHeaderValueCollection")
extern class HttpHeaderValueCollection<T> {
	var Count(default, never):Int;
	var IsReadOnly(default, never):Bool;
	function Add(item:T):Void;
	function Clear():Void;
	function Contains(item:T):Bool;
	function CopyTo(array:cs.NativeArray<T>, arrayIndex:Int):Void;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<T>;
	function ParseAdd(input:String):Void;
	function Remove(item:T):Bool;
	function ToString():String;
	function TryParseAdd(input:String):Bool;
}
