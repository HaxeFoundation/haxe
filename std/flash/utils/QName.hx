package flash.utils;

extern final class QName {
	var localName(get,never) : String;
	var uri(get,never) : Dynamic;
	function new(?namespace : Dynamic, ?name : Dynamic) : Void;
	private function get_localName() : String;
	private function get_uri() : Dynamic;
}
