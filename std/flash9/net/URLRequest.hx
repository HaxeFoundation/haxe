package flash.net;

extern class URLRequest {
	function new(?url : String) : Void;
	var contentType : String;
	var data : Dynamic;
	var method : String;
	var requestHeaders : Array<Dynamic>;
	var url : String;
}
