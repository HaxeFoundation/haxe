package flash.utils;

extern class Proxy {
	function new() : Void;
	function callProperty(name : Dynamic /* ...arguments */) : Void;
	function deleteProperty(name : Dynamic) : Bool;
	function getDescendants(name : Dynamic) : Void;
	function getProperty(name : Dynamic) : Void;
	function hasProperty(name : Dynamic) : Bool;
	function isAttribute(name : Dynamic) : Bool;
	function nextName(index : Int) : String;
	function nextNameIndex(index : Int) : Int;
	function nextValue(index : Int) : Void;
	function setProperty(name : Dynamic, value : Dynamic) : Void;
}
