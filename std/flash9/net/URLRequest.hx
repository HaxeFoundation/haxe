package flash.net;

extern class URLRequest {
	var contentType : String;
	var data : Dynamic;
	/** added in FP 9.0.115 **/
	var digest : String;
	var method : String;
	var requestHeaders : Array<URLRequestHeader>;
	var url : String;
	function new(?url : String) : Void;
}
