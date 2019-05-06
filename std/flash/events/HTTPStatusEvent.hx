package flash.events;

extern class HTTPStatusEvent extends Event {
	@:flash.property var redirected(get,set) : Bool;
	@:flash.property @:require(flash10_1) var responseHeaders(get,set) : Array<Dynamic>;
	@:flash.property @:require(flash10_1) var responseURL(get,set) : String;
	@:flash.property var status(get,never) : Int;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, status : Int = 0, redirected : Bool = false) : Void;
	private function get_redirected() : Bool;
	private function get_responseHeaders() : Array<Dynamic>;
	private function get_responseURL() : String;
	private function get_status() : Int;
	private function set_redirected(value : Bool) : Bool;
	private function set_responseHeaders(value : Array<Dynamic>) : Array<Dynamic>;
	private function set_responseURL(value : String) : String;
	@:require(flash10_1) static final HTTP_RESPONSE_STATUS : String;
	static final HTTP_STATUS : String;
}
