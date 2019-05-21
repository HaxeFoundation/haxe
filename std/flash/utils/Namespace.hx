package flash.utils;

extern final class Namespace {
	@:flash.property var prefix(get,never) : Dynamic;
	@:flash.property var uri(get,never) : String;
	function new(?prefix : Dynamic, ?uri : Dynamic) : Void;
	private function get_prefix() : Dynamic;
	private function get_uri() : String;
}
