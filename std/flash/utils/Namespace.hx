package flash.utils;

extern final class Namespace {
	var prefix(get,never) : Dynamic;
	var uri(get,never) : String;
	function new(?prefix : Dynamic, ?uri : Dynamic) : Void;
	private function get_prefix() : Dynamic;
	private function get_uri() : String;
}
