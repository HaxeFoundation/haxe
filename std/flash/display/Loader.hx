package flash.display;

extern class Loader extends DisplayObjectContainer {
	@:flash.property var content(get,never) : DisplayObject;
	@:flash.property var contentLoaderInfo(get,never) : LoaderInfo;
	@:flash.property @:require(flash10_1) var uncaughtErrorEvents(get,never) : flash.events.UncaughtErrorEvents;
	function new() : Void;
	function close() : Void;
	private function get_content() : DisplayObject;
	private function get_contentLoaderInfo() : LoaderInfo;
	private function get_uncaughtErrorEvents() : flash.events.UncaughtErrorEvents;
	function load(request : flash.net.URLRequest, ?context : flash.system.LoaderContext) : Void;
	function loadBytes(bytes : flash.utils.ByteArray, ?context : flash.system.LoaderContext) : Void;
	function unload() : Void;
	@:require(flash10) function unloadAndStop(gc : Bool = true) : Void;
}
