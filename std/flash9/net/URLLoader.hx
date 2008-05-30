package flash.net;

extern class URLLoader extends flash.events.EventDispatcher {
	var bytesLoaded : UInt;
	var bytesTotal : UInt;
	var data : Dynamic;
	var dataFormat : URLLoaderDataFormat;
	function new(?request : URLRequest) : Void;
	function close() : Void;
	function load(request : URLRequest) : Void;
}
