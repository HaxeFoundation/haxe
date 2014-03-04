package flash.net;

@:final extern class URLRequest {
	var contentType : String;
	var data : Dynamic;
	var digest : String;
	var method : String;
	var requestHeaders : Array<URLRequestHeader>;
	var url : String;
	function new(?url : String) : Void;
	function useRedirectedURL(sourceRequest : URLRequest, wholeURL : Bool = false, ?pattern : Dynamic, ?replace : String) : Void;
}
