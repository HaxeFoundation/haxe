package flash.utils;

@:final extern class Namespace {
	var prefix(default,never) : Dynamic;
	var uri(default,never) : String;
	function new(?prefix : Dynamic, ?uri : Dynamic) : Void;
}
