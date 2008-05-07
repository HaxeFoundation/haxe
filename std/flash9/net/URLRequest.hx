package flash.net;

extern class URLRequest {
	function new(?url : String) : Void;
	var contentType : String;
	var data : Dynamic;
	
	/** added in FP 9.0.115 **/
	var digest : String;
	
	var method : String;
	var requestHeaders : Array<URLRequestHeader>;
	var url : String;
}
