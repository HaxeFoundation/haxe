package flash.net;

extern class URLVariables implements Dynamic {
	function new(?source : String) : Void;
	function decode(source : String) : Void;
	function toString() : String;
}
