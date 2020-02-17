package flash.net;

extern final class URLRequest {
	@:flash.property var contentType(get,set) : String;
	@:flash.property var data(get,set) : Dynamic;
	@:flash.property var digest(get,set) : String;
	@:flash.property var method(get,set) : String;
	@:flash.property var requestHeaders(get,set) : Array<URLRequestHeader>;
	@:flash.property var url(get,set) : String;
	function new(?url : String) : Void;
	private function get_contentType() : String;
	private function get_data() : Dynamic;
	private function get_digest() : String;
	private function get_method() : String;
	private function get_requestHeaders() : Array<URLRequestHeader>;
	private function get_url() : String;
	private function set_contentType(value : String) : String;
	private function set_data(value : Dynamic) : Dynamic;
	private function set_digest(value : String) : String;
	private function set_method(value : String) : String;
	private function set_requestHeaders(value : Array<URLRequestHeader>) : Array<URLRequestHeader>;
	private function set_url(value : String) : String;
	function useRedirectedURL(sourceRequest : URLRequest, wholeURL : Bool = false, ?pattern : Dynamic, ?replace : String) : Void;
}
