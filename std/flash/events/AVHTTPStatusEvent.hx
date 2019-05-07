package flash.events;

extern class AVHTTPStatusEvent extends Event {
	@:flash.property var responseHeaders(get,set) : Array<Dynamic>;
	@:flash.property var responseURL(get,set) : String;
	@:flash.property var status(get,never) : Int;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, status : Int = 0, ?responseUrl : String, ?responseHeaders : Array<Dynamic>) : Void;
	private function get_responseHeaders() : Array<Dynamic>;
	private function get_responseURL() : String;
	private function get_status() : Int;
	private function set_responseHeaders(value : Array<Dynamic>) : Array<Dynamic>;
	private function set_responseURL(value : String) : String;
	static final AV_HTTP_RESPONSE_STATUS : String;
}
