package flash.net;

@:final extern class URLRequestHeader {
	var name : String;
	var value : String;
	function new(?name : String, ?value : String) : Void;
}
