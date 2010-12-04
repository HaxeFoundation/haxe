package flash.display;

extern class Loader extends DisplayObjectContainer {
	var content(default,null) : DisplayObject;
	var contentLoaderInfo(default,null) : LoaderInfo;
	function new() : Void;
	function close() : Void;
	function load(request : flash.net.URLRequest, ?context : flash.system.LoaderContext) : Void;
	function loadBytes(bytes : flash.utils.ByteArray, ?context : flash.system.LoaderContext) : Void;
	function unload() : Void;
}
