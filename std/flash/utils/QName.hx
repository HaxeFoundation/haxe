package flash.utils;

extern final class QName {
	var localName(default,never) : String;
	var uri(default,never) : Dynamic;
	function new(?namespace : Dynamic, ?name : Dynamic) : Void;
}
