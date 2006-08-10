package flash.net;

extern class URLVariables {
	function new(?source : String) : Void;
	function decode(source : String) : Void;
	function toString() : String;
	private function _unescape(value : String) : String;
}
