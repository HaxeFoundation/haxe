package flash.utils;

@:final extern class QName {
	var localName(default,null) : String;
	var uri(default,null) : Dynamic;
	function new(?namespace : Dynamic, ?name : Dynamic) : Void;
}
