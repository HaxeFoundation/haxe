package flash.utils;

extern class Proxy {
	function new() : Void;
	@:ns("http://www.adobe.com/2006/actionscript/flash/proxy") function callProperty(name : Dynamic, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Dynamic;
	@:ns("http://www.adobe.com/2006/actionscript/flash/proxy") function deleteProperty(name : Dynamic) : Bool;
	@:ns("http://www.adobe.com/2006/actionscript/flash/proxy") function getDescendants(name : Dynamic) : Dynamic;
	@:ns("http://www.adobe.com/2006/actionscript/flash/proxy") function getProperty(name : Dynamic) : Dynamic;
	@:ns("http://www.adobe.com/2006/actionscript/flash/proxy") function hasProperty(name : Dynamic) : Bool;
	@:ns("http://www.adobe.com/2006/actionscript/flash/proxy") function isAttribute(name : Dynamic) : Bool;
	@:ns("http://www.adobe.com/2006/actionscript/flash/proxy") function nextName(index : Int) : String;
	@:ns("http://www.adobe.com/2006/actionscript/flash/proxy") function nextNameIndex(index : Int) : Int;
	@:ns("http://www.adobe.com/2006/actionscript/flash/proxy") function nextValue(index : Int) : Dynamic;
	@:ns("http://www.adobe.com/2006/actionscript/flash/proxy") function setProperty(name : Dynamic, value : Dynamic) : Void;
}
