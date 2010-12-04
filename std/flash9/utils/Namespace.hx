package flash.utils;

@:final extern class Namespace {
	var prefix(default,null) : Dynamic;
	var uri(default,null) : String;
	function new(?prefix : Dynamic, ?uri : Dynamic) : Void;
}
