package flash.display;

extern class Loader extends DisplayObjectContainer {
	var content(default,never) : DisplayObject;
	var contentLoaderInfo(default,never) : LoaderInfo;
	@:require(flash10_1) var uncaughtErrorEvents(default,never) : flash.events.UncaughtErrorEvents;
	function new() : Void;
	function close() : Void;
	function load(request : flash.net.URLRequest, ?context : flash.system.LoaderContext) : Void;
	function loadBytes(bytes : flash.utils.ByteArray, ?context : flash.system.LoaderContext) : Void;
	function unload() : Void;
	@:require(flash10) function unloadAndStop(gc : Bool = true) : Void;
}
